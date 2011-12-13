#+
#  Name:
#     Ccd::scrollbox

#  Purpose:
#     Defines a class of "listbox with scrollbars".

#  Language:
#     TCL

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This class description defines methods and configurations for
#     creating a listbox with scrollbars a "scrollbox". The scrollbox may
#     have scrollbars positioned either at the top or bottom and left or
#     right of the listbox. The scrollbars are arranged to give a
#     Motif-like look (with spaces in the corners) and may be
#     reconfigured at any time.

#  Configuration Options:
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

#  Inheritance:
#     This class inherits Ccd::base and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Invocations:
#        Ccd::scrollbox window [-option value]...
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

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#         creates the scrollbox widget with a default configuration,
#         except when overridden by command line options.
#     destructor
#        Destroys the scrollbox, invoked by the "delete" method.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#         is given then the current value of any known option is returned
#         in a form similar (but not identical to) the Tk widget command.
#     insert index text
#        Inserts a line of text with the given index. "index" can
#         be 0 or end which inserts at the beginning and at the end.
#     clear first [last]
#        Clears a range of items from the listbox. If first is "all"
#         then all lines are deleted. If only first is given then this
#         clears a single line. "last" may be set as end.
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
#         scrollbarplaces). This is really an internal method and
#         shouldn't be used.
#     listname
#        Returns the name of the listbox widget.
#     scrollbarnames places
#        Returns the names of any scrollbars at the given places ("left"
#         "right", "top" or "bottom").
#     select option args
#        Controls the selection in the listbox. "option" is any of those
#         which are valid for a listbox, args are the qualifiers.
#     curselection
#        Returns a list of the indices of any items selected in the
#         listbox.
#     vmoveto pos
#        Moves the viewable region vertically according to the value of
#        the pos argument: 0 means the top is visible and 1 means the
#        bottom is visible.
#     hmoveto pos
#        Moves the viewable region horizontally according to the value of
#        the pos argument: 0 means the left is visible and 1 means the
#        right is visible.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2000-2001 Central Laboratory of the Research
#     Councils. Copyright (C) 2006 Particle Physics & Astronomy
#     Research Council. All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     19-MAR-1994 (PDRAPER):
#        Original version.
#     4-MAY-1995 (PDRAPER):
#        Started move to Tk4. Commented out ::rename in destructor, no
#        longer needed. Listboxes do not support -geometry.
#     5-MAY-1995 (PDRAPER):
#        Now uses Tk4 select modes.
#     21-JUL-1995 (PDRAPER):
#        Added sethelp method.
#     15-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     5-JUL-2001 (MBT):
#        Added hmoveto and vmoveto methods.
#     27-JAN-2006 (PDRAPER):
#        Update for itcl::class syntax.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   itcl::class Ccd::scrollbox {


#  Inheritances:
      inherit Ccd::base

#.

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the Ccd::scrollbox class and
#  configures it with the default and command-line options.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { args } {

#  Create listbox.
         CCDTkWidget List list listbox $oldthis.list

#  Define sub-component widgets for configuration via the wconfig
#  method.
         set widgetnames($Oldthis:list) $List
         set widgetfocus($Oldthis:list) $List

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
         eval configure $args
         configure -height          $height
         configure -width           $width
         configure -anchor          $anchor
         configure -scrollbarplaces $scrollbarplaces
         configure -singleselect    $singleselect

#  Pack listbox
         pack $list -expand true -fill both
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Insert line of text method.
      method insert { index args } {
         eval $List insert $index $args
      }

#  Clear range of lines of text method.
      method clear { args } {
         if { [lindex $args 0 ] != "all" } {
            eval $List delete $args
         } else {
            $List delete 0 end
         }
      }

#  Return the range of lines with current selection.
      method curselection {} {
         return [$List curselection]
      }

#  Get information back from the listbox.
      method get { args } {
         if { [lindex $args 0 ] == "all" } {
            set value [$List get 0 end]
            return $value
         } else {
            set value [$List get $args]
            return $value
         }
      }

#  Method for assigning context help to all the elements of the object.
      method sethelp {docname label} {
         if { $exists } {

#  Rather than work out which configuration setting we're using, just
#  extract all the possible widget names and check that they exist.
            foreach oneof [array names widgetnames "$Oldthis:*"] {
               set Name $widgetnames($oneof)
               set name [CCDPathOf $Name]
               if [winfo exists $name] {
                  Ccd::base::sethelp $Name $docname $label
               }
            }
         }
      }

#  Size of listbox contents.
      method size {} {
         return [$List size]
      }

#  Scrollbar positioning
      method vmoveto { pos } {
         set bar [lindex [scrollbarnames {left right}] 0]
         if { $bar != "" } {
            eval [$bar cget -command] moveto $pos
         }
      }
      method hmoveto { pos } {
         set bar [lindex [scrollbarnames {top bottom}] 0]
         if { $bar != "" } {
            eval [$bar cget -command] moveto $pos
         }
      }

#  Internal method for creating and or re-packing scrollbars
      method _repack places  {

#  First remove all scrollbars and packing frames, make the listbox
#  forget any scrolling commands.
         foreach side "left right" {
            if { [ info exists Scrolls($side) ] } {
               set scroll [CCDPathOf $Scrolls($side)]
               pack forget $scroll

#  Destroy the scrollbar and make the listbox forget about the scrollcommand.
               destroy $scroll
               $List configure -yscrollcommand {}
               unset Scrolls($side)
            }
         }
         foreach side "top bottom" {
            if { [ info exists Scrolls($side) ] } {
               set scroll [CCDPathOf $Scrolls($side)]
               set frame [CCDPathOf $Frames($side)]
               pack forget $scroll

#  Destroy the scrollbar and make the listbox forget about the scrollcommand.
               destroy $scroll
               destroy $frame
               $List configure -xscrollcommand {}
               unset Scrolls($side)
               unset Frames($side)
            }

#  Destroy any packing frames.
            if { [ info exists Frames($side.left) ] } {
               set frame [CCDPathOf $Frames($side.left)]
               pack forget $frame
               destroy $frame
               unset Frames($side.left)
            }
            if { [ info exists Frames($side.right) ] } {
               set frame [CCDPathOf $Frames($side.right)]
               pack forget $frame
               destroy $frame
               unset Frames($side.right)
            }
         }

#  And unpack the listbox itself (so that rearrangement is easy).
         pack forget $list

#  Find out if any packing frames are required (this fill the corners of
#  the base frame so that scrollbars look natural).
         set haveleft   [string match *left*   $places]
         set haveright  [string match *right*  $places]
         set havetop    [string match *top*    $places]
         set havebottom [string match *bottom* $places]

#  Create the necessary scrollbars. Append any names etc. to the
#  sub-widget control variables.
         if { $haveleft } {
            CCDTkWidget Scroll scroll \
               scrollbar $oldthis.scrollleft \
                  -orient vertical \
                  -command "$List yview"
            set Scrolls(left) $Scroll
            $List configure -yscrollcommand "$Scroll set"
            set widgetnames($Oldthis:scrollleft) $Scroll
         }

         if { $haveright } {
            CCDTkWidget Scroll scroll \
               scrollbar $oldthis.scrollright \
                  -orient vertical \
                  -command "$List yview"
            set Scrolls(right) $Scroll
            $List configure -yscrollcommand "$Scroll set"
            set widgetnames($Oldthis:scrollright) $Scroll
         }

         if { $havetop } {
            CCDTkWidget Frame frame frame $oldthis.top -borderwidth 0
            set Frames(top) $Frame
            if { $haveleft } {
               CCDTkWidget Frame1 frame1 \
                  frame $frame.left \
                         -width [winfo reqwidth [CCDPathOf $Scrolls(left)]]
               pack $frame1 -side left
               set Frames(top.left) $Frame1
            }
            CCDTkWidget Scroll scroll \
               scrollbar $frame.scrolltop \
                  -orient horizontal \
                  -command "$List xview"
            pack $scroll -side left -fill x -expand true
            $List configure -xscrollcommand "$Scroll set"
            set widgetnames($Oldthis:scrolltop) $Scroll
            set Scrolls(top) $Scroll
            if { $haveright } {
               CCDTkWidget Frame1 frame1 \
                  frame $frame.right \
                         -width [winfo reqwidth [CCDPathOf $Scrolls(right)]]
               pack $frame1 -side left
               set Frames(top.right) $Frame1
            }
         }

         if { $havebottom } {
            CCDTkWidget Frame frame frame $oldthis.bottom -borderwidth 0
            set Frames(bottom) $Frame
            if { $haveleft } {
               CCDTkWidget Frame1 frame1 \
                  frame $frame.left \
                         -width [winfo reqwidth [CCDPathOf $Scrolls(left)]]
               pack $frame1 -side left
               set Frames(bottom.left) $Frame1
            }
            CCDTkWidget Scroll scroll \
               scrollbar $frame.scrollbottom \
                  -orient horizontal \
                  -command "$List xview"
            pack $scroll -side left -fill x -expand true
            $List configure -xscrollcommand "$Scroll set"
            set widgetnames($Oldthis:scrollbottom) $Frame
            set Scrolls(bottom) $Scroll
            if { $haveright } {
               CCDTkWidget Frame1 frame1 \
                  frame $frame.right \
                         -width [winfo reqwidth [CCDPathOf $Scrolls(right)]]
               pack $frame1 -side left
               set Frames(bottom.right) $frame1
            }
         }

#  Perform packing of main elements (need to do this now to get into
#  correct places.
         if { $havetop }    {
            pack [CCDPathOf $Frames(top)]         -side top    -fill x
         }
         if { $havebottom } {
            pack [CCDPathOf $Frames(bottom)]      -side bottom -fill x
         }
         if { $haveleft }   {
            pack [CCDPathOf $Scrolls(left)]       -side left   -fill y
         }
         if { $haveright }  {
            pack [CCDPathOf $Scrolls(right)]      -side right  -fill y
         }
         pack $list -expand true -fill both
      }

#  Method to return the name of the listbox widget.
      method listname {} {
         return $oldthis.list
      }

#  Method to control selection. This has the same options as the listbox
#  selection.
      method select { option args } {
         eval $List selection $option $args
      }

#  Method to return the names of the scrollbar widgets
      method scrollbarnames { places } {
         foreach side $places {
            if { ! [ regexp (left|right|top|bottom) $side ] } {
               error "Unknown scrollbar placement \"$side\", should be top bottom left or right"
            }
         }
         foreach side $places {
            if { [ info exists Scrolls($side) ] } {
               lappend barnames $Scrolls($side)
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
      public variable scrollbarplaces { right bottom } {
         foreach side $scrollbarplaces {
            if { ! [ regexp (left|right|top|bottom) $side ] } {
               error "Unknown scrollbar placement \"$side\", should be top bottom left or right"
            }
         }

#  Only proceed if the object exists (this means that constructor has
#  been invoked).
         if { $exists } {
            _repack $scrollbarplaces
         }
      }

#  If a label has been requested then add one.
      public variable label {} {
         if { $label != {} } {
            if { $exists } {
               if { ![winfo exists $labelwidget] } {
                  CCDTkWidget Labelwidget labelwidget \
                     label $oldthis.label -text "$label"
                  pack $labelwidget -side top -anchor $anchor
               } else {
                  $labelwidget configure -text "$label"
               }
               set widgetnames($Oldthis:label) $Labelwidget
               set widgetfocus($Oldthis:label) $Labelwidget
               _repack $scrollbarplaces
            }
         } else {

#  Remove existing label.
            if { [ winfo exists $labelwidget ] } {
               pack forget $labelwidget
	       destroy $labelwidget
               unset widgetnames($Oldthis:label)
               unset widgetfocus($Oldthis:label)
	    }
         }
      }

#  Set anchor position of the label.
      public variable anchor w {
         configure -label $label
      }

#  Can more than one entry be selected at a time?
      public variable singleselect 1 {
         if { $exists } {
            if { $singleselect } {
               $List configure -selectmode browse
            } else {
               $List configure -selectmode extended
            }
         }
      }

#  Is the selection exportable? If not may have more than one selection
#  (one for each instance), otherwise the selection is the X11 one.
      public variable exportselect 1 {
         if { $exists } {
            $List configure -exportselection $exportselect
         }
      }

#  Height of listbox.
      public variable height 10 {
         if { $exists } {
            $List configure -height $height
         }
      }

#  Width of listbox.
      public variable width 20 {
         if { $exists } {
            $List configure -width $width
         }
      }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Names of widgets
      protected variable List
      protected variable list ""
      protected variable Scrolls
      protected variable Frames
      protected variable Labelwidget
      protected variable labelwidget ""


#  End of class defintion.
   }

# $Id$

#+
#  Name:
#     Scrollbox

#  Type of Module:
#     [incr Tk] class

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
#        Scrollbox window [-option value]...
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
#     This class inherits FrameWidget and its methods and configuration
#     options, which are not directly occluded by those specified here.


#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2008 Science and Technology Facilities Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     27-MAR-1996 (PWD):
#        Original version, based on CCDPACK Ccd_scrollbox.
#     9-JUL-1996 (PWD):
#        Converted to itk2.0.
#     {enter_further_changes_here}

#-
itk::usual Scrollbox {}

itcl::class gaia::Scrollbox {

   #  Inheritances:
   inherit util::FrameWidget

   # Constructor:
   # ------------
   constructor {args} {

      #  Create listbox.
      itk_component add List {
         listbox $w_.list
      } {
         keep -height -width -exportselection -listvariable
      }

      #  Set default configurations.
      eval itk_initialize $args

      #  Pack listbox
      pack $itk_component(List) -expand true -fill both
   }

   #  Methods:
   #  --------

   #  Insert line of text method.
   public method insert { index args } {
      eval $itk_component(List) insert $index $args
   }

   #  Clear range of lines of text method.
   public method clear { args } {
      if { [lindex $args 0 ] != "all" } {
         eval $itk_component(List) delete $args
      } else {
         $itk_component(List) delete 0 end
      }
   }

   #  Return the range of lines with current selection.
   public method curselection {} {
      return [$itk_component(List) curselection]
   }

   #  Get information back from the listbox.
   public method get { args } {
      if { [lindex $args 0 ] == "all" } {
         set value [$itk_component(List) get 0 end]
         return $value
      } else {
         set value [$itk_component(List) get $args]
         return $value
      }
   }

   #  Size of listbox contents.
   public method size {} {
      return [$itk_component(List) size]
   }

   #  Make sure we can see the given item.
   public method see {index} {
      $itk_component(List) see $index
   }

   #  Internal method for creating and or re-packing scrollbars
   private method _repack places  {
      #  First remove all scrollbars and packing frames, make the listbox
      #  forget any scrolling commands.
      foreach side "Left Right" {
         if { [info exists itk_component($side)] } {
            pack forget $itk_component($side)

            #  Destroy the scrollbar and make the listbox forget about
            #  the scrollcommand.
            destroy $itk_component($side)
            $itk_component(List) configure -yscrollcommand {}
         }
      }
      foreach side "Top Bottom" {
         if { [info exists itk_component($side)] } {
            pack forget $itk_component($side)

            #  Destroy the scrollbar and make the listbox forget about
            #  the scrollcommand.
            destroy $itk_component($side)
            destroy $itk_component(${side}Frame)
            $itk_component(List) configure -xscrollcommand {}
         }

         #  Destroy any packing frames.
         if { [info exists itk_component(Top${side})] } {
            pack forget $itk_component(Top${side})
            destroy $itk_component(Top${side})
         }
         if { [info exists itk_component(Bottom${side})] } {
            pack forget itk_component(Bottom${side})
            destroy itk_component(Bottom${side})
         }
      }

      #  And unpack the listbox itself (so that rearrangement is easy).
      pack forget $itk_component(List)

      #  Find out if any packing frames are required (this fill the corners of
      #  the base frame so that scrollbars look natural).
      set haveleft   [string match *left*   $places]
      set haveright  [string match *right*  $places]
      set havetop    [string match *top*    $places]
      set havebottom [string match *bottom* $places]

      #  Create the necessary scrollbars. Append any names etc. to the
      #  sub-widget control variables.
      if { $haveleft } {
         itk_component add Left {
            scrollbar $w_.scrollleft \
               -orient vertical \
               -command [code $itk_component(List) yview]
         }
         $itk_component(List) configure \
            -yscrollcommand [code $itk_component(Left) set]
      }

      if { $haveright } {
         itk_component add Right {
            scrollbar $w_.scrollright \
               -orient vertical \
               -command [code $itk_component(List) yview]
         }
         $itk_component(List) configure \
            -yscrollcommand [code $itk_component(Right) set]
      }

      if { $havetop } {
         itk_component add TopFrame {
            frame $w_.top -borderwidth 0
         }
         if { $haveleft } {
            itk_component add TopLeft {
               frame $itk_component(TopFrame).left \
                  -width [winfo reqwidth $itk_component(Left)]
            }
            pack $itk_component(TopLeft) -side left
         }
         itk_component add Top {
            scrollbar $itk_component(TopFrame).scrolltop \
               -orient horizontal \
               -command [code $itk_component(List) xview]
         }
         pack $itk_component(Top) -side left -fill x -expand true
         $itk_component(List) configure \
            -xscrollcommand [code $itk_component(Top) set]
         if { $haveright } {
            itk_component add TopRight {
               frame $itk_component(TopFrame).right \
                  -width [winfo reqwidth $itk_component(Right)]
            }
            pack $itk_component(TopRight) -side left
         }
      }

      if { $havebottom } {
         itk_component add BottomFrame {
            frame $w_.bottom -borderwidth 0
         }
         if { $haveleft } {
            itk_component add BottomLeft {
               frame $itk_component(BottomFrame).left \
                  -width [winfo reqwidth $itk_component(Left)]
            }
            pack $itk_component(BottomLeft) -side left
         }
         itk_component add Bottom {
            scrollbar $itk_component(BottomFrame).scrollbottom \
               -orient horizontal \
               -command [code $itk_component(List) xview]
         }
         pack $itk_component(Bottom) -side left -fill x -expand true
         $itk_component(List) configure \
            -xscrollcommand [code $itk_component(Bottom) set]
         if { $haveright } {
            itk_component add BottomRight {
               frame $itk_component(BottomFrame).right \
                  -width [winfo reqwidth $itk_component(Right)]
            }
            pack $itk_component(BottomRight) -side left
         }
      }

      #  Perform packing of main elements (need to do this now to get into
      #  correct places.
      if { $havetop } {
         pack $itk_component(TopFrame) -side top -fill x
      }
      if { $havebottom } {
         pack $itk_component(BottomFrame) -side bottom -fill x
      }
      if { $haveleft } {
         pack $itk_component(Left) -side left -fill y
      }
      if { $haveright } {
         pack $itk_component(Right) -side right -fill y
      }
      pack $itk_component(List) -expand true -fill both
   }

   #  Method to return the name of the listbox widget, use this to set
   #  bindings etc.
   public method listname {} {
      return $itk_component(List)
   }

   #  Method to control selection. This has the same options as the listbox
   #  selection.
   public method select { option args } {
      eval $itk_component(List) selection $option $args
   }

   #  Method to return the names of the scrollbar widgets
   public method scrollbarnames { places } {
      foreach side $places {
         if { ! [ regexp (left|right|top|bottom) $side ] } {
            error "Unknown scrollbar placement \"$side\", should be top bottom left or right"
         }
      }
      foreach side $places {
         if { [ winfo exists $w_.scroll$side ] } {
            lappend barnames $w_.scroll$side
         }
      }
      if { [ info exists barnames ] } {
         return "$barnames"
      } else {
         return {}
      }
   }

   #  Configuration options:
   #  ----------------------

   #  Insert and pack the required scrollbars. Remove any existing
   #  scrollbars first.
   itk_option define -scrollbarplaces scrollbarplaces ScrollBarPlaces \
              {right bottom} {
      foreach side $itk_option(-scrollbarplaces) {
            if { ! [ regexp (left|right|top|bottom) $side ] } {
               error "Unknown scrollbar placement \"$side\", should be top bottom left or right"
            }
         }
         _repack $itk_option(-scrollbarplaces)
      }

   #  If a label has been requested then add one.
   itk_option define -label label Label {} {
      if { $itk_option(-label) != {} } {
         if { ! [info exists itk_component(Label)] } {
	    itk_component add Label {
               label $w_.label -text "$itk_option(-label)"
	    }
	    pack $itk_component(Label) -side top -anchor $itk_option(-anchor)
	    _repack $itk_option(-scrollbarplaces)
	 } else {
	    $itk_component(Label) configure -text "$itk_option(-label)"
	 }
      } else {
         #  Remove existing label.
	 if { [info exists itk_component(Label)] } {
	    if { [ winfo exists $itk_component(Label) ] } {
	       pack forget $itk_component(Label)
	       destroy $itk_component(Label)
	    }
	 }
      }
   }

   #  Set anchor position of the label.
   itk_option define -anchor anchor Anchor w {
      configure -label $itk_option(-label)
   }

   #  Can more than one entry be selected at a time?
   itk_option define -singleselect singleselect Singleselect 1 {
      if { $itk_option(-singleselect) } {
         $itk_component(List) configure -selectmode browse
      } else {
         $itk_component(List) configure -selectmode extended
      }
   }

   #  Protected variables:
   #  --------------------

   #  End of class defintion.
}


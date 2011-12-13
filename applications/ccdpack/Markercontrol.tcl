   class Markercontrol {
#+
#  Name:
#     Markercontrol

#  Purpose:
#     Control widget for keeping track of markers.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Description:
#     This control provides to the calling code a widget which can
#     plot points, remove them, and keep track of where they are.
#     To the user (the person running the GUI) it provides a method
#     of specifying the appearance and index numbers of marked points.
#
#     Communication is done using view coordinates.  To know the
#     details of the canvas on which it is plotting, this widget
#     requires the name of the canvas with which it is associated
#     and a function to use for converting from view to canvas
#     coordinates.

#  Public Methods:
#     addpoint vx vy ?ipoint?
#        Adds a point at the given view coordinates to the list of
#        marked points.  If the ipoint parameter is not given then
#        the index of the new point will be selected in a sensible,
#        perhaps user-selectable, fashion from unused values.
#        The return value of this method is the index of the point plotted.
#           - vx       -- X coordinate of new point in view coordinates
#           - vy       -- Y coordinate of new point in view coordinates
#           - ?ipoint? -- Index of new point
#
#     clearpoints
#        Clears the contents of the marked points list, and erases all
#        the corresponding marked points from the display.
#        This may be more efficient than calling the removepoint method
#        on each point individually.
#
#     points
#        Returns a list of currently marked points on the image.
#        The returned list has one element for each marked point, each
#        of these elements is of the form {index xpos ypos}.  Xpos and
#        ypos are in view coordinates.
#
#     refreshpoints
#        Draws all the points which are currently in the points list.
#        This method can be called after clearing some or all of the
#        canvas to ensure that all the points which are current get
#        marked on it.
#
#     removepoint ipoint
#        Removes the point with index ipoint from the points list.
#           - ipoint   -- Index in the points list of the point to be removed

#  Public Variables (Configuration Options):
#     canvas
#        The name of the canvas item on which the markers are to be drawn.
#
#     shownumcontrol
#        A boolean variable indicating whether the Marknumcontrol widget,
#        which shows and controls the number of the next widget to be
#        plotted, will be visible (true) or hidden (false).  Defaults to
#        true.
#
#     view2canvcmd
#        A command which will map view coordinates to canvas coordinates
#        (for the canvas specified by the 'canvas' configuration option).
#        If vx and vy are view coordinates, the command
#        [ $view2canvcmd $vx $vy ] should return a two-element list
#        giving the corresponding canvas coordinates.
#
#     uselabels = boolean
#        If true, then the index number of each point will be displayed
#        alongside the marker when it is plotted and it will be
#        possible for the user to select the index of the next point
#        to be plotted.  Otherwise an index is chosen automatically
#        from the list of unused ones.  The default is false.
#
#     Markercontrol also inherits all the public variables of the Control
#     widget.

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

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
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Inheritance:
      inherit Control


########################################################################
#  Constructor.
########################################################################
      constructor { args } {
         itk_component add control {
            frame [ childsite ].control
         }
         itk_component add marknumcontrol {
            marknumcontrol $itk_component(control).markernum -max 0
         }
         itk_component add markstylecontrol {
            markstylecontrol $itk_component(control).markerstyle
         }

#  Set up some private instance variables.
         set markernum $itk_component(marknumcontrol)
         set markerstyle $itk_component(markstylecontrol)
         set tagname "_tag_$this:"

#  Pack the component widgets.
         pack $markerstyle -side left -fill both -expand 1
         pack $itk_component(control) -fill both -expand 1

#  Do configuration.  This is a somewhat messy business because this
#  Control widget contains a couple of other Control widgets; normally
#  Control widgets only expect to be owned by top-level mega-widgets.
         set configuring 1
         eval itk_initialize $args
         $markerstyle configure -command \
            [ code "$this configure -value \[ $markerstyle cget -value \]" ]
         configure -shownumcontrol $shownumcontrol
         set configuring 0
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method addpoint { vx vy { ipoint "" } } {
#-----------------------------------------------------------------------
#  Add a point to the list of marked points.

#  Use default value for point index if none is specified.
         if { $ipoint == "" } {
            set ipoint [ $markernum next ]
         }

#  Check that we have a valid point index.
         if { $ipoint <= 0 } {
            bell
            return -1
         }

#  If any point with this index exists in the list already, remove it.
         removepoint $ipoint

#  Set a unique tag for this point.
         set tag [ gettag $ipoint ]

#  Draw the point on the canvas.
         marker $vx $vy [ list $tagname $tag ] $ipoint

#  Add this point to the list of points we know about.
         lappend points [ list $ipoint $vx $vy $tag ]
         $markernum use $ipoint

#  Rearrange the elements of the points list to be in index-ascending order
#  (this isn't really necessary, but makes the output list look tidier
#  and makes some processing easier).
         set points [ lsort -integer -index 0 $points ]

#  Return the index of the point which was added.
         return $ipoint
      }


#-----------------------------------------------------------------------
      public method removepoint { ipoint } {
#-----------------------------------------------------------------------
#  Remove a point from the list and erase it from the canvas.

#  If any point in the list has the index ipoint, then remove it from the
#  list and from the canvas.
         set i 0
         foreach p $points {
            if { [ lindex $p 0 ] == $ipoint } {
               set tag [ lindex $p 3 ]
               $canvas delete $tag
               set points [ lreplace $points $i $i ]
            }
            incr i
         }

#  Possibly modify the number of the next marker to be plotted according
#  to whether we've just opened up a suitable gap in the list.
         $markernum unuse $ipoint
      }


#-----------------------------------------------------------------------
      public method clearpoints {} {
#-----------------------------------------------------------------------
#  Removes any extant points from the points list and the canvas.
         if { [ llength $points ] } {
            $canvas delete $tagname
            set points {}
         }
         $markernum clear
      }


#-----------------------------------------------------------------------
      public method refreshpoints {} {
#-----------------------------------------------------------------------
         if { [ llength $points ] } {
            $canvas delete $tagname
            foreach p $points {
               marker [ lindex $p 1 ] [ lindex $p 2 ] \
                      "[lindex $p 3] $tagname" [ lindex $p 0 ]
            }
         }
      }


#-----------------------------------------------------------------------
      public method points {} {
#-----------------------------------------------------------------------
         set rp {}
         foreach p $points {
            lappend rp [ list [ lindex $p 0 ] [ lindex $p 1 ] [ lindex $p 2 ] ]
         }
         return $rp
      }


#-----------------------------------------------------------------------
      public method marker { vx vy { tags "" } { label "" } } {
#-----------------------------------------------------------------------
         set cpos [ view2canv $vx $vy ]
         set cx [ lindex $cpos 0 ]
         set cy [ lindex $cpos 1 ]
         set taglist ""
         if { $tags != "" } {
            eval lappend taglist $tags
         }
         if { ! $uselabels } {
            set label ""
         }
         $markerstyle draw $canvas $cx $cy $taglist $label
      }


#-----------------------------------------------------------------------
      public method gettag { ipoint } {
#-----------------------------------------------------------------------
         return $tagname$ipoint
      }


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable view2canvcmd { error "no -view2canvcmd value" } {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable canvas { error "no -canvas value" } {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $markernum configure -state "normal"
            $markerstyle configure -state "normal"
         } elseif { $state == "disabled" } {
            $markernum configure -state "disabled"
            $markerstyle configure -state "disabled"
         }
      }


#-----------------------------------------------------------------------
      public variable shownumcontrol { 1 } {
#-----------------------------------------------------------------------
         if { $shownumcontrol } {
            pack $markernum -after $markerstyle \
                            -side left -fill both -expand 1
         } else {
            pack forget $markernum
         }
      }


#-----------------------------------------------------------------------
      public variable uselabels 1 {
#-----------------------------------------------------------------------
         if { $uselabels } {
            pack $markernum -after $markerstyle -side left -fill both -expand 1
         } else {
            pack forget $markernum
         }
      }


#-----------------------------------------------------------------------
      public variable value { } {
#-----------------------------------------------------------------------
         refreshpoints
         if { ! $configuring } {
            set configuring 1
            $markerstyle configure -value $value
            set configuring 0
         }
      }



########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method view2canv { vx vy } {
#-----------------------------------------------------------------------
         if { [ catch "$view2canvcmd $vx $vy" result ] } {
            error $result
         } else {
            return $result
         }
      }


########################################################################
#  Private variables.
########################################################################

      private variable configuring 0    ;# Prevent recursion
      private variable markernum        ;# Marknumcontrol widget
      private variable markerstyle      ;# Markstylecontrol widget
      private variable points {}        ;# List of points {index x y tags}
      private variable tagname ""       ;# Prefix for marker tags

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Markercontrol {
      keep -background -cursor -foreground
   }


########################################################################
#  Constructor alias
########################################################################

   proc markercontrol { pathname args } {
      uplevel Markercontrol $pathname $args
   }


# $Id$

#+
#  Name:
#     GaiaTabStops

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Widget to allow the interactive placement of tab stops

#  Description:
#     This class defines a widget that displays a line of text, which
#     can have "tab stops" interactively moved into place along
#     it. The number of tab stops is initially controlled by a
#     configuration option, however, new stops may be added
#     interactively.
#
#     New stops are added by pressing the blue tab-stop, this creates
#     a new stop that can then be positioned. Configuration options
#     allow for callbacks whenever a tab-stop is repositioned.

#  Invocations:
#
#        GaiaTabStops object_name [configuration options]
#
#     This creates an instance of a GaiaTabStops object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#     See itk_option define statements below.

#  Methods:
#     See method declarations below.

#  Inheritance:
#     FrameWidget

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
#     10-APR-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaTabStops {}

itcl::class gaia::GaiaTabStops {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the display elements. These are a scrolled canvas
      #  region with a label, a creator blue triangle and a series of
      #  red triangles pointing to the tab positions.
      itk_component add canvas {
         ::iwidgets::scrolledcanvas $w_.canvas \
            -autoresize 1 \
            -vscrollmode none \
            -hscrollmode dynamic \
            -xscrollincrement 5m
      }
      pack $itk_component(canvas) -side top -fill both -expand 1
      set canvas_ $itk_component(canvas)

      #  Use label to display text.
      itk_component add label {
         label $canvas_.textlabel \
            -textvariable [scope itk_option(-text)]
      } {
         rename -font -textfont textFont TextFont
      }

      #  Evaluate options (note no keep sections allowed beyond this point).
      eval itk_initialize $args
      set label_ $itk_component(label)
      update_label_size_


      #  Work out a geometry for all triangles.
      set trioffset_ [expr int($entheight_)+1]
      set triheight_ [expr $trioffset_+$triheight_]

      #  Set height of canvas to include label and triangles.
      $canvas_ configure -height [expr $triheight_+30]

      #  Add the "source" tabstop triangle. Pressing <1> on this
      #  generates a new marker, moving the cursor also moves the new
      #  marker.
      set xo 5
      set id [$canvas_ create polygon \
                 $xo $trioffset_ \
                 [expr $xo-$triwidth_] $triheight_ \
                 [expr $xo+$triwidth_] $triheight_ \
                 -tag $w_.tab_source -fill blue]
      $canvas_ bind $id <ButtonPress-1> [code $this add_stop_]
      $canvas_ bind $id <B1-Motion> [code $this move_last_stop_ %x]
      $canvas_ bind $id <ButtonRelease-1> [code $this update_stops_]
      $canvas_ bind $id <Enter> \
         [code $this short_help {Press <1> to create new marker}]
      $canvas_ bind $id <Leave> [code $this short_help {}]

      #  Add text label to the canvas.
      set labelid_ [$canvas_ create window [expr int($step_*1.5)] 0 \
                       -anchor nw -width $entwidth_ \
                       -height $entheight_ -window $label_]

      #  Dragging <1> off visible canvas region autoscrolls.
      set ic [$canvas_ component canvas]
      bind $ic <B1-Leave> [code $this autoscan_ start $ic %x]
      bind $ic <B1-Enter> [code $this autoscan_ stop $ic 0]
      bind $ic <ButtonRelease-1> [code $this autoscan_ stop $ic 0]
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Move a tab-stop marker.
   protected method move_stop_ {id x} {
      set cx [$canvas_ canvasx $x $step_]
      if { $cx > $step_ && $cx < $entwidth_+$step_ } {
         $canvas_ coords $id \
            $cx $trioffset_ \
            [expr $cx-$triwidth_] $triheight_ \
            [expr $cx+$triwidth_] $triheight_
      }
      set last_id_ $id
   }

   #  Move the tab-stop that has just been created or moved.
   protected method move_last_stop_ {x} {
      move_stop_ $last_id_ $x
   }

   #  Remove a tab-stop.
   protected method remove_stop_ {id} {
      $canvas_ delete $id
      update_stops_
   }

   #  Add a tab-stop, returns the canvas identifier.
   protected method add_stop_ {} {
      set xo 10
      set id [$canvas_ create polygon \
                 $xo $trioffset_ \
                 [expr $xo-$triwidth_] $triheight_ \
                 [expr $xo+$triwidth_] $triheight_ \
                 -tag $w_.tabs -fill red]
      $canvas_ bind $id <B1-Motion> [code $this move_stop_ $id %x]
      $canvas_ bind $id <2> [code $this remove_stop_ $id]
      $canvas_ bind $id <ButtonRelease-1> [code $this update_stops_]
      set last_id_ $id
      return $id
   }

   #  Control the autoscan function when dragged off canvas.
   protected method autoscan_ {do w x} {
      if { $do == "start" } {
         #  Start autoscan, cancel existing ones.
         if { $afterid_ != {} } {
            after cancel $afterid_
            set afterid_ {}
         }
         set width [winfo width $w]
         if {$x >= $width } {
            $canvas_ xview scroll 1 units
            move_last_stop_ $width
         } elseif {$x < 0} {
            $canvas_ xview scroll -1 units
            move_last_stop_ 0
         }
         eval set afterid_ [after 50 [code $this autoscan_ $do $w $x]]
      } else {
         #  Stop autoscan.
         if { $afterid_ != {} } {
            after cancel $afterid_
            set afterid_ {}
         }
      }
   }

   #  Set the position of a tab-stop by character index.
   protected method set_stop_ {id index} {

      #  Get x position for tab-stop.
      set cx [expr ($index+1)*$step_]
      if { $cx < $step_ || $cx > $entwidth_+$step_ } {

         #  Off range so put at initial position.
         set cx 10
      }
      $canvas_ coords $id \
         $cx $trioffset_ \
         [expr $cx-$triwidth_] $triheight_ \
         [expr $cx+$triwidth_] $triheight_
   }

   #  Update the size of the label widget (in response to new text).
   protected method update_label_size_ {} {
      set entheight_ [winfo reqheight $label_]
      set entwidth_ [winfo reqwidth $label_]
      set step_ [expr $entwidth_/[string length $itk_option(-text)]]
      if { $labelid_ != {} } {
         $canvas_ itemconfigure $labelid_ -width $entwidth_ \
            -height $entheight_
      }
   }

   #  Update the stop positions in characters (rather than canvas) and
   #  initiate the changed command if needed.
   protected method update_stops_ {} {
      catch {unset indexes_}
      foreach id [$canvas_ find withtag $w_.tabs] {
         lassign [$canvas_ coords $id] x
         lappend indexes_ [expr int($x/$step_)-1]
      }
      set indexes_ [lsort -integer $indexes_]
      if { $itk_option(-change_cmd) != {} } {
         eval $itk_option(-change_cmd) {$indexes_}
      }
   }

   #  Return the character positions of the stops.
   public method getindices {} {
      return $indexes_
   }

   #  Set the positions of all tab stops. The given positions are
   #  a character based index string or list.
   public method setindices {values} {
      foreach id [$canvas_ find withtag $w_.tabs] {
         $canvas_ delete $id
      }
      foreach index $values {
         set id [add_stop_]
         set_stop_ $id $index
      }
   }

   #  Set the "state" of the widget. Normal or disabled.
   protected method update_state_ {} {
      if { $itk_option(-state) == "normal" } {
         catch {blt::busy release $w_}
         catch {configure -foreground $itk_option(-enabledforeground)}
      } else {
         catch {blt::busy hold $w_ -cursor {}}
         catch {configure -foreground $itk_option(-disabledforeground)}
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The text shown in the label widget. Also updates the label
   #  widget displayed size.
   itk_option define -text text Text "gaia::GaiaTabStops" {
      if { $label_ != {} } {
         update_label_size_
      }
   }

   #  Command to execute when the stops are changed.
   itk_option define -change_cmd change_cmd Change_cmd {}

   #  State of widget -- normal or disabled.
   itk_option define -state state State normal {
      update_state_
   }

   #  Colour of disabled text.
   itk_option define -disabledforeground disabledforeground \
      Disabledforeground gray90

   #  Colour of enabled text.
   itk_option define -enabledforeground enabledforeground \
      Enabledforeground black

   #  Protected variables: (available to instance)
   #  --------------------

   #  Quick name of canvas.
   protected variable canvas_ {}

   #  Quick name of label.
   protected variable label_ {}

   #  Canvas identifier of label widget.
   protected variable labelid_ {}

   #  Shape of pointer triangles.
   protected variable triwidth_ 3
   protected variable triheight_ 10
   protected variable trioffset_ 20

   #  Width and height (in pixels) of label widget.
   protected variable entwidth_ 100
   protected variable entheight_ 20

   #  Size of a character in pixels.
   protected variable step_ 1

   #  Character indices of the stops.
   protected variable indexes_ {}

   #  Canvas id of last tab-stop.
   protected variable last_id_ {}

   #  Identifier of after command.
   protected variable afterid_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

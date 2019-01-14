#+
#  Name:
#     StarCanvasObject

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class of object for controlling an item drawn using a
#     StarCanvasDraw object.

#  Description:
#     This class provides the basic functionality for controlling an
#     item. It provides the basic draw facilities, controls the
#     selection/deselection colours, handles call backs on when
#     actions occur on the item etc. See the methods available.

#  Invocations:
#
#        StarCanvasObject object_name [configuration options]
#
#     This creates an instance of a StarCanvasObject object. The return is
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
#     See public variable declarations below.

#  Methods:
#     See method declarations below.

#  Inheritance:
#     This class is a base class and inherits from no other classes.

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
#     8-MAY-1996 (PWD):
#        Original version.
#     5-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarCanvasObject {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      eval configure $args

      #  Increment the number of items (this is a common number shared
      #  amongst all items.
      incr number_of_objects

      #  Object doesn't exist until drawn.
      set state_ new
   }

   #  Destructor:
   #  -----------
   destructor  {
      incr number_of_objects -1
      set notify_change_cmd {}

      #  Delete the graphic item (someone else may have removed it so
      #  be careful).
      if { $canvas_id_ != {} } {
         catch { $canvasdraw remove_notify_cmd $canvas_id_ }
         catch { $canvasdraw delete_object $canvas_id_ }
         set canvas_id_ {}
      }
      if { $notify_delete_cmd != {} } {
         eval $notify_delete_cmd
         unset notify_delete_cmd
      }

      #  Undo any grip bindings.
      catch {
         $canvas bind grip <ButtonRelease-1> {}
         $canvas bind grip <B1-Motion> {}
      }
   }

   #  Methods:
   #  --------

   #  Create an item and allow it to be resized.
   method create_and_resize {cmd} {
      if { "$canvasdraw" != {} } {
         set create_cmd_ $cmd
         $canvasdraw set_drawing_mode $mode "[code $this created]"
      }
   }

   #  Create an item using the current coordinates.
   method create_no_resize {cmd tcoords} {
      if { "$canvasdraw" != {} } {
         set create_cmd_ $cmd
         set coords $tcoords
         create
      }
   }

   #  Draw an item with the current values.
   method create {} {
      if { "$canvasdraw" != {} } {

         #  Need to store and later restore current configure this is
         #  overriden by update called during these commands. Note
         #  this method assumes the first two elements of coords are a
         #  position.
         set tcoords $coords
         set x1 [lindex $coords 0]
         set y1 [lindex $coords 1]
         $canvasdraw set_drawing_mode $mode "[code $this created]"
         $canvasdraw create_object $x1 $y1
         $canvasdraw create_done $x1 $y1
         set coords $tcoords

         #  If object is a polygon then need to set all the points
         #  correctly (the updates for this object do not read from
         #  the canvas position as it requires complete control of the
         #  positions and number).
         if { $mode == "pointpoly" } {
            set elements [llength $coords]
            for {set i 0} {$i < $elements} {incr i} {
               set x [lindex $coords $i]
               incr i
               set y [lindex $coords $i]
               $canvasdraw add_pointpoly_point $x $y 0
            }
         } else {
            eval $canvas coords $canvas_id_ $tcoords
         }
         update $canvas_id_ move
         notify_change 0
      }
   }

   #  Finally created the item. Record details and add call
   #  backs for interactive updates etc. Execute the requested
   #  creation command so that the user knows that the aperture is
   #  created. Add this object as one of an tag set.
   method created {id args} {
      set state_ drawn
      set selected_ 1
      set canvas_id_ $id
      $canvasdraw add_notify_cmd $id "[code $this update $id]" 0
      #  Stop the creation of any more objects while this one completes
      #  (can be asynchronous).
      $canvasdraw set_drawing_mode anyselect

      update $id create
      add_bindings_ $id
      $canvas addtag $tag withtag $id
      if { $create_cmd_ != {} } {
         eval $create_cmd_ $id
         set create_cmd_ {}
      }
      notify_change 0
   }

   #  Update item coords on creation, movement, resize or delete.
   method update {id mode} {
      switch -exact $mode {
         create {
            set coords [$canvas coords $id]

            #  Now receive movement and resize updates.
            $canvasdraw add_notify_cmd $id "[code $this update $id]" 0
         }
         move -
         resize {
            set coords [$canvas coords $id]
         }
         delete {
            delete object $this
         }
      }
   }

   #  Add the bindings to this object that are appropriate to the
   #  current settings. Also sets up the call backs for when object is
   #  selected or deselected by canvasdraw.
   method add_bindings_ {id} {
      $canvas bind $id <ButtonRelease-1> "+[code $this notify_change 0]"
      $canvas bind $id <ButtonRelease-1> "+[code $this add_grip_bindings_]"
      $canvas bind $id <Double-Button-1> "+[code $this show_properties]"
      $canvasdraw add_selected_notify $id [code $this update_selection]
      $canvas bind $id <B1-Motion> "+[code $this notify_change 1]"
   }

   #  When <1> is pressed add the necessary bindings to the grips that
   #  appear.
   method add_grip_bindings_ {} {
      set b1 {}
      catch { set b1 "[$canvas bind grip <ButtonRelease-1>]" }
      if { ! [string match "*$this notify_change*" "$b1"] } {
         $canvas bind grip <ButtonRelease-1> [code $this notify_change 0]
      }
      set b1m {}
      catch { set b1m "[$canvas bind grip <B1-Motion>]"}
      if { ! [string match "*$this notify_change*" "$b1m"] } {
         $canvas bind grip <B1-Motion> [code $this notify_change 1]
      }
   }

   #  Notify that a show properties request has been made, if needed.
   method show_properties {} {
      if {$show_properties_cmd != {} } {
         eval $show_properties_cmd
      }
   }

   #  Notify that object may have changed. cont indicates if update is
   #  of a continous nature.
   method notify_change {cont} {
      if { $notify_change_cmd != {} } {
         if { $cont && $continuous_updates || !$cont } {
            eval $notify_change_cmd
         }
      }
   }

   #  Object selection state is changed.
   method update_selection {state} {
      if { $state == "selected" } {
         selected
      } else {
         deselected
      }
   }

   #  Object is selected so colour it.
   method selected {} {
      switch $mode {
         line -
         polygon -
         column -
         row -
         pixel {
            $canvas itemconfigure $canvas_id_ -fill $selected_colour
         }
         default {
            $canvas itemconfigure $canvas_id_ -outline $selected_colour
         }
      }
      set selected_ 1
      if { $notify_selected_cmd != {} } {
         eval $notify_selected_cmd selected
      }
   }

   #  Object is deselected so colour it.
   method deselected {} {
      switch $mode {
         line -
         polygon -
         column -
         row -
         pixel {
            $canvas itemconfigure $canvas_id_ -fill $deselected_colour
         }
         default {
            $canvas itemconfigure $canvas_id_ -outline $deselected_colour
         }
      }
      set selected_ 0
      if { $notify_selected_cmd != {} } {
         eval $notify_selected_cmd deselected
      }
   }

   #  Convert from image coordinates to grid coordinates, i.e.
   #  remove the origin and 1/2 pixel.
   method grid_coord { x y } {
      if { $rtdimage != {} } {
         if { $use_origins } {
            $rtdimage origin xo yo
         } else {
            set xo 1.0
            set yo 1.0
         }
         set x [expr $x+1.5-$xo]
         set y [expr $y+1.5-$yo]
      }
      return "$x $y"
   }

   #  Convert from canvas coordinates to image coordinates.
   method image_coord { x y } {
      if { $rtdimage != {} } {
         $rtdimage convert coords $x $y canvas x y image
         if { $use_origins } {
            $rtdimage origin xo yo
         } else {
            set xo 1.0
            set yo 1.0
         }
         set x [expr $x-1.5+$xo]
         set y [expr $y-1.5+$yo]
      }
      return "$x $y"
   }

   #  Convert an angle measured on the canvas (+X through +Y) into
   #  an angle in image coords (Y flipped).
   method image_angle { angle {xpos {}} {ypos {}}} {
      # return [expr fmod(180.0-$angle,180)]
      set rad [expr $angle*0.017453292519943295]
      if { $xpos == {} || $ypos == {} } {
         lassign [$canvas coords $canvas_id_] xpos ypos
      }
      set dx [expr sin($rad)+$xpos]
      set dy [expr cos($rad)+$ypos]
      $rtdimage convert coords $dx $dy canvas ndx ndy image
      $rtdimage convert coords $xpos $ypos canvas nxcen nycen image
      set ndx [expr $ndx-$nxcen]
      set ndy [expr $ndy-$nycen]
      set angle [expr fmod(90.0-atan2($ndy,$ndx)*57.295779513082323,180.0)]
      return $angle
   }

   #  Convert a canvas distance to an image distance (along X axis).
   #  Need to be careful that axes interchange hasn't taken place, in
   #  which case the value we want can be in the out "y" system.
   method image_dist { dist } {
      if { $rtdimage != {} } {
         $rtdimage convert dist $dist 0 canvas dist1 dist2 image
         set dist [expr ($dist1+$dist2)]
      }
      return "$dist"
   }

   #  Convert from image coordinates to canvas coordinates.
   method canvas_coord { x y } {
      if { $rtdimage != {} } {
         if { $use_origins } {
            $rtdimage origin xo yo
         } else {
            set xo 1.0
            set yo 1.0
         }
         set x [expr $x+1.5-$xo]
         set y [expr $y+1.5-$yo]
         $rtdimage convert coords $x $y image x y canvas
      }
      return "$x $y"
   }

   #  Convert an angle measured on the image (+X through +Y) into
   #  an angle in canvas coords (Y flipped).
   method canvas_angle { angle {xpos {}} {ypos {}}} {
      set rad [expr $angle*0.017453292519943295]
      if { $xpos == {} || $ypos == {} } {
         lassign [$canvas coords $canvas_id_] xpos ypos
      }
      $rtdimage convert coords $xpos $ypos canvas nxcen nycen image
      set dx [expr sin($rad)+$nxcen]
      set dy [expr cos($rad)+$nycen]
      $rtdimage convert coords $dx $dy image ndx ndy canvas
      set ndx [expr $ndx-$xpos]
      set ndy [expr $ndy-$ypos]
      set angle [expr fmod(90.0-atan2($ndy,$ndx)*57.295779513082323,180.0)]
      return $angle
   }

   #  Convert an image distance to a canvas distance (along X axis
   #  which may be swapped on output).
   method canvas_dist { dist } {
      if { $rtdimage != {} } {
         $rtdimage convert dist $dist 0 image dist1 dist2 canvas
         set dist [expr ($dist1+$dist2)]
      }
      return "$dist"
   }

   #  Redraw the object with the current sizes.
   method redraw {} {
      if {$canvas != {} } {
         eval $canvas coords $canvas_id_ $coords
      }
   }

   #  Return whether the object is selected or not.
   method is_selected {} {
      return $selected_
   }

   #  Procedures: (access common values)
   #  -----------

   #  Return total number of rectangles that exist.
   proc total_objects {} {
      return $number_of_objects
   }

   #  Set the value of a common variable.
   proc set_common {variable value} {
      if {[info exists $variable]} {
         set $variable $value
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Drawing mode. Needs to be one that StarCanvasDraw understands.
   #  This should only really be set once per object.
   public variable mode {rectangle} {
   }

   #  The coords of the item.
   public variable coords {} {
      if { $state_ != "new" } {
         redraw
      }
   }

   #  Command to execute when a "show_properties" event occurs.
   public variable show_properties_cmd {} {}

   #  Command to execute if item is updated.
   public variable notify_change_cmd {} {}

   #  Command to execute if item is deleted.
   public variable notify_delete_cmd {} {}

   #  Command to execute when object is selected or deselected. The
   #  command will have a single argument "selected" or "deselected"
   #  appended to the command.
   public variable notify_selected_cmd {}

   #  Set colours selected and deselected objects.
   public variable selected_colour white {}
   public variable deselected_colour green {}

   #  Tag for object. Assumes this is part of a group.
   public variable tag StarCanvasObjects {}

   #  Whether to provide continuous updates on object or not.
   public variable continuous_updates 1 {}

   #  Name of StarCanvasDraw object that controls the overlay graphics
   #  of the object.
   public variable canvasdraw {} {}

   #  Name of canvas.
   public variable canvas {} {}

   #  Name of GaiaImageCtrl type object for converting aperture size
   #  into displayed canvas size and coordinates to image.
   public variable rtdimage {} {}

   #  Whether to include the NDF origin information in any image coordinates.
   public variable use_origins 1 {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  State of object, new until it is drawn.
   protected variable state_ new

   #  Whether object is selected or not.
   protected variable selected_ 0

   #  Id of object aperture.
   protected variable canvas_id_ {}

   #  Command to execute after interactive object is completed.
   protected variable create_cmd_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Number of objects created.
   common number_of_objects 0

#  End of class definition.
}

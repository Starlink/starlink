#+
#  Name:
#     StarCanvasDraw
#
#  Type of Module:
#     [incr Tk] class
#
#  Purpose:
#     Adds the functionality required for applications to use the
#     CanvasDraw class.
#
#  Description:
#     See methods and configurations for additional functionality.
#     Basically it adds the new canvas items, ellipses, circles,
#     point editable polygon, rotating box, pixel and pixel wide
#     row and columns.
#
#  Invocations:
#
#        StarCanvasDraw object_name [configuration options]
#
#     This creates an instance of a StarCanvasDraw object. The return is
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
#
#  Inheritance:
#     Inherits CanvasDraw.
#
#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     NG: Norman Gray (Starlink, Glasgow)
#
#  Copyright:
#     Copyright 2000, Central Laboratory of the Research Councils
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
#
#  History:
#     9-MAY-1996 (PWD):
#        Original version.
#     3-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     23-APR-1998 (PWD):
#        Converted to SkyCat V2.0.7.
#     22-FEB-1999 (PWD):
#        Merged Allan's changes into main GAIA branch:
#          Ported to work with new CanvasDraw version.
#          Made clipping an option.
#          Changed tag "selected" to "$w_.selected", and added new tag
#          "$w_.objects" in addition to the more general tag "objects".
#          Updated method create_object.
#     5-DEC-1999 (NG):
#        Added `square' and `arc' drawing modes.  These aren't used
#        in the version of the toolbox which implements only the
#        ELLPRO/FOU interface.
#     1-FEB-2000 (PWD):
#        Changed Norman's sector to "arc" and enabled.
#     25-MAY-2006 (PWD):
#        Added `xrange' and `yrange' drawing modes.
#-

#.

itk::usual StarCanvasDraw {}

itcl::class gaia::StarCanvasDraw {

   #  Inheritances:
   #  -------------
   inherit util::CanvasDraw

   #  Constructor:
   #  ------------
   constructor {args} {
      set CanvasDraw::drawing_modes_ $drawing_modes_
   } {
      eval itk_initialize $args
   }

   # This method is called after the constructor have completed.

   method init {} {
      util::CanvasDraw::init

      # Set some options in the base class.
      # (Since we have reimplemented rotation in C, we don't need the inherited Tcl version.)
      configure -withrotate 0

      #  Define pixel_width_
      update_pixel_width
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Override method clear to be more careful about how delete events
   #  are delivered.
   method clear {} {
      deselect_objects
      foreach id [$canvas_ find withtag objects] {
         delete_object $id
      }
      foreach id [$canvas_ find withtag $w_.objects] {
         delete_object $id
      }
   }


   #  Set the width of a pixel in the RTD image.
   method update_pixel_width {} {
      if { $itk_option(-rtdimage) != {} } {
         set pixel_width_ [lindex [$itk_option(-rtdimage) scale] 0]
         if { $pixel_width_ == {} } {
            set pixel_width_ 1
         }
      }
   }

   #  Create the canvas item menu, adds a lower command to control the
   #  selection of overlaid items that obscure each other.
   method make_object_menu {} {
      util::CanvasDraw::make_object_menu
      $object_menu_ add command -label "Lower Selected Item(s)" \
         -command [code $this lower_selected_objects]
   }

   #  Lower the selected objects in the canvas, but not lower than the image!
   method lower_selected_objects {} {
      $canvas_ lower $w_.selected
      $canvas_ raise $w_.selected $itk_option(-lowestitem)
   }

   #  Delete an object from the canvas.
   method delete_object {id} {
      util::CanvasDraw::delete_object $id
      catch {unset types_($id)}
   }

   #  Return list of currently selected objects.
   method list_selected {} {
      set objects ""
      foreach i [$canvas_ find withtag $w_.selected] {
         lappend objects $i
      }
      return $objects
   }

   #  Set the type of object to draw (line, oval, etc.).  If
   #  create_cmd is specified, it is called with the coordinates of
   #  the new object when its creation is completed.

   method set_drawing_mode {type {create_cmd ""}} {
      #  XXX need this as interaction with cube bindings is weird (out of
      #  order, see <B1-Motion> first!).
      if { ! [info exists obj_id_] } {
         set obj_id_ {}
      }
      util::CanvasDraw::set_drawing_mode $type $create_cmd
      switch -exact $drawing_mode_ {
         ellipse -
         rotbox {
            bind $canvas_ <1> \
               [code eval $this create_object $canvasX_ $canvasY_]
            bind $canvas_ <ButtonRelease-1> \
               [code eval $this check_done $canvasX_ $canvasY_ update_ellipse]
            bind $canvas_ <B1-Motion> \
               [code eval $this update_ellipse $canvasX_ $canvasY_]
            bind $canvas_ <Shift-B1-Motion> \
               [code eval $this update_ellipse $canvasX_ $canvasY_]
            bind $canvas_ <Double-ButtonPress-1> \
               [code eval $this create_done $canvasX_ $canvasY_]
         }
         circle {
            bind $canvas_ <ButtonPress-1> \
               [code eval $this create_object $canvasX_ $canvasY_]
            bind $canvas_ <ButtonRelease-1> \
               [code eval $this check_done $canvasX_ $canvasY_ update_circle]
            bind $canvas_ <B1-Motion> \
               [code eval $this update_circle $canvasX_ $canvasY_]
            bind $canvas_ <Shift-B1-Motion> \
               [code eval $this update_circle $canvasX_ $canvasY_]
            bind $canvas_ <Double-ButtonPress-1> \
               [code eval $this create_done $canvasX_ $canvasY_]
         }
         square {
             bind $canvas_ <1> \
                     [code eval $this create_object $canvasX_ $canvasY_]
             bind $canvas_ <ButtonRelease-1> \
                     [code eval $this check_done $canvasX_ $canvasY_ update_square]
             bind $canvas_ <B1-Motion> \
                     [code eval $this update_square $canvasX_ $canvasY_]
             bind $canvas_ <Shift-B1-Motion> \
                     [code eval $this update_square $canvasX_ $canvasY_]
             bind $canvas_ <Double-ButtonPress-1> \
                     [code eval $this create_done $canvasX_ $canvasY_]
         }
         arc {
             bind $canvas_ <1> \
                     [code eval $this create_object $canvasX_ $canvasY_]
             bind $canvas_ <ButtonRelease-1> \
                     [code eval $this check_done $canvasX_ $canvasY_ update_arc]
             bind $canvas_ <B1-Motion> \
                     [code eval $this update_arc $canvasX_ $canvasY_]
             bind $canvas_ <Shift-B1-Motion> \
                     [code eval $this update_arc $canvasX_ $canvasY_]
             bind $canvas_ <Double-ButtonPress-1> \
                     [code eval $this create_done $canvasX_ $canvasY_]
         }
         column {
            bind $canvas_ <1> \
               [code eval $this create_object $canvasX_ $canvasY_]
            bind $canvas_ <ButtonRelease-1> \
               [code eval $this create_done $canvasX_ $canvasY_]
            bind $canvas_ <B1-Motion> \
               [code eval $this update_column $canvasX_ $canvasY_]
            bind $canvas_ <Shift-B1-Motion> \
               [code eval $this update_column $canvasX_ $canvasY_]
         }
         row {
            bind $canvas_ <1> \
               [code eval $this create_object $canvasX_ $canvasY_]
            bind $canvas_ <ButtonRelease-1> \
               [code eval $this create_done $canvasX_ $canvasY_]
            bind $canvas_ <B1-Motion> \
               [code eval $this update_row $canvasX_ $canvasY_]
            bind $canvas_ <Shift-B1-Motion> \
               [code eval $this update_row $canvasX_ $canvasY_]
         }
         pixel {
            bind $canvas_ <1> \
               [code eval $this create_object $canvasX_ $canvasY_]
            bind $canvas_ <ButtonRelease-1> \
               [code eval $this create_done $canvasX_ $canvasY_]
            bind $canvas_ <B1-Motion> \
               [code eval $this update_pixel $canvasX_ $canvasY_]
            bind $canvas_ <Shift-B1-Motion> \
               [code eval $this update_pixel $canvasX_ $canvasY_]
         }
         pointpoly {
            bind $canvas_ <1> \
               [code eval $this create_object $canvasX_ $canvasY_]
            bind $canvas_ <ButtonRelease-1> \
               [code eval $this add_pointpoly_point $canvasX_ $canvasY_]
            bind $canvas_ <B1-Motion> \
               [code eval $this update_pointpoly $canvasX_ $canvasY_]
            bind $canvas_ <Shift-B1-Motion> \
               [code eval $this update_pointpoly $canvasX_ $canvasY_]
            bind $canvas_ <Double-ButtonPress-1> \
               [code eval $this create_done $canvasX_ $canvasY_]
            bind $canvas_ <Control-1> \
               [code eval $this create_done $canvasX_ $canvasY_]
         }
         xrange {
             bind $canvas_ <1> \
                     [code eval $this create_object $canvasX_ $canvasY_]
             bind $canvas_ <ButtonRelease-1> \
                     [code eval $this check_done $canvasX_ $canvasY_ update_xrange]
             bind $canvas_ <B1-Motion> \
                     [code eval $this update_xrange $canvasX_ $canvasY_]
             bind $canvas_ <Shift-B1-Motion> \
                     [code eval $this update_xrange $canvasX_ $canvasY_]
             bind $canvas_ <Double-ButtonPress-1> \
                     [code eval $this create_done $canvasX_ $canvasY_]
         }
         yrange {
             bind $canvas_ <1> \
                     [code eval $this create_object $canvasX_ $canvasY_]
             bind $canvas_ <ButtonRelease-1> \
                     [code eval $this check_done $canvasX_ $canvasY_ update_yrange]
             bind $canvas_ <B1-Motion> \
                     [code eval $this update_yrange $canvasX_ $canvasY_]
             bind $canvas_ <Shift-B1-Motion> \
                     [code eval $this update_yrange $canvasX_ $canvasY_]
             bind $canvas_ <Double-ButtonPress-1> \
                     [code eval $this create_done $canvasX_ $canvasY_]
         }
      }
   }

   #  Called when creation is done. Override to test if a value for obj_id_
   #  has been set yet. This may not be true when the button release is seen
   #  before the button press. This seems to unexpectable happen when the
   #  canvas objects have a binding to <1>.
   public method create_done {x y} {
      if { $obj_id_ == "" } {
         return
      }
      util::CanvasDraw::create_done $x $y
   }

   #  Select the given object by drawing little grips on it.
   method select_object {id {any 0}} {
      if {$drawing_} {
         return
      }
      set draw $itk_option(-show_selection_grips)
      if { [info exists types_($id)] } {
         switch $types_($id) {
            circle {
               if { $draw } { draw_circle_selection_grips $id }
               $canvas_ addtag $w_.selected withtag $id
            }
            ellipse -
            rotbox {
               if { $draw } { draw_ellipse_selection_grips $id }
               $canvas_ addtag $w_.selected withtag $id
            }
            square {
                if {$draw} {draw_square_selection_grips $id}
                $canvas_ addtag $w_.selected withtag $id
            }
            arc {
                if {$draw} {draw_arc_selection_grips $id}
                $canvas_ addtag $w_.selected withtag $id
            }
            column -
            row -
            pixel {
               if { $draw } { $canvas_ addtag $w_.selected withtag $id }
            }
            pointpoly {
               if { $draw } { draw_pointpoly_selection_grips $id }
               $canvas_ addtag $w_.selected withtag $id
            }
            default {
               util::CanvasDraw::select_object $id $any
            }
         }
      } else {
         #  We have no knowledge of this object so just pass on
         #  responsibility.
         util::CanvasDraw::select_object $id $any
      }

      #  Notify that object is selected, if asked.
      if {[info exists notify_selected_($id)]} {
         eval "$notify_selected_($id) selected"
      }
   }

   #  Draw the selection grips for an ellipse. These are at the
   #  ends of the semimajor and semiminor axes.

   method draw_ellipse_selection_grips {id} {
      foreach side {e n} {
         set sel_id [$canvas_ create rectangle 0 0 \
                     $itk_option(-gripwidth) \
                     $itk_option(-gripwidth) \
                        -tags "grip grip.$id grip.$id.$side" \
                        -fill $itk_option(-gripfill) \
                        -outline $itk_option(-gripoutline)]

         $canvas_ bind $sel_id <Any-Enter> \
            [code $canvas_ configure -cursor $itk_option(-linecursor)]
         $canvas_ bind $sel_id <Any-Leave> \
            [code $this reset_cursor]
         $canvas_ bind $sel_id <1> \
            [code eval $this mark $canvasX_ $canvasY_]
         $canvas_ bind $sel_id <Shift-1> \
            [code eval $this mark $canvasX_ $canvasY_]
         $canvas_ bind $sel_id <ButtonRelease-1> \
            [code $this end_resize_ellipse $id]
         $canvas_ bind $sel_id <Shift-ButtonRelease-1> \
            [code $this end_resize_ellipse $id]
         $canvas_ bind $sel_id <B1-Motion> \
            [code eval $this resize_ellipse $id $canvasX_ $canvasY_ $side]
         $canvas_ bind $sel_id <Shift-B1-Motion> \
            [code eval $this resize_ellipse $id $canvasX_ $canvasY_ $side]
      }
      adjust_ellipse_selection $id
   }


   #  Draw the selection grip for a circle. This is at the end of the X axes.

   method draw_circle_selection_grips {id} {
      set sel_id [$canvas_ create rectangle 0 0 \
                     $itk_option(-gripwidth)  $itk_option(-gripwidth) \
                     -tags "grip grip.$id" \
                     -fill $itk_option(-gripfill) \
                     -outline $itk_option(-gripoutline)]

      $canvas_ bind $sel_id <Any-Enter> \
         [code $canvas_ configure -cursor $itk_option(-linecursor)]
      $canvas_ bind $sel_id <Any-Leave> \
         [code $this reset_cursor]
      $canvas_ bind $sel_id <1> \
         [code eval $this mark $canvasX_ $canvasY_]
      $canvas_ bind $sel_id <Shift-1> \
         [code eval $this mark $canvasX_ $canvasY_]
      $canvas_ bind $sel_id <ButtonRelease-1> \
         [code $this end_resize_circle $id]
      $canvas_ bind $sel_id <Shift-ButtonRelease-1> \
         [code $this end_resize_circle $id]
      $canvas_ bind $sel_id <B1-Motion> \
         [code eval $this resize_circle $id $canvasX_ $canvasY_]
      $canvas_ bind $sel_id <Shift-B1-Motion> \
         [code eval $this resize_circle $id $canvasX_ $canvasY_]
      adjust_circle_selection $id
   }

   #  Draw the selection grip for a square. This is at the end of the X axis.

   method draw_square_selection_grips {id} {
      set sel_id [$canvas_ create rectangle 0 0 \
                     $itk_option(-gripwidth)  $itk_option(-gripwidth) \
                     -tags "grip grip.$id" \
                     -fill $itk_option(-gripfill) \
                     -outline $itk_option(-gripoutline)]

      $canvas_ bind $sel_id <Any-Enter> \
         [code $canvas_ configure -cursor $itk_option(-linecursor)]
      $canvas_ bind $sel_id <Any-Leave> \
         [code $this reset_cursor]
      $canvas_ bind $sel_id <1> \
         [code eval $this mark $canvasX_ $canvasY_]
      $canvas_ bind $sel_id <Shift-1> \
         [code eval $this mark $canvasX_ $canvasY_]
      $canvas_ bind $sel_id <ButtonRelease-1> \
         [code $this end_resize_square $id]
      $canvas_ bind $sel_id <Shift-ButtonRelease-1> \
         [code $this end_resize_square $id]
      $canvas_ bind $sel_id <B1-Motion> \
         [code eval $this resize_square $id $canvasX_ $canvasY_]
      $canvas_ bind $sel_id <Shift-B1-Motion> \
         [code eval $this resize_square $id $canvasX_ $canvasY_]
      adjust_square_selection $id
   }

   #  Draw the selection grips for a arc.
   #  These are in the middle of the arc, and the middle of one of the lines

   method draw_arc_selection_grips {id} {
      foreach side {arc line} {
         set sel_id [$canvas_ create rectangle 0 0 \
                     $itk_option(-gripwidth) \
                     $itk_option(-gripwidth) \
                        -tags "grip grip.$id grip.$id.$side" \
                        -fill $itk_option(-gripfill) \
                        -outline $itk_option(-gripoutline)]

         $canvas_ bind $sel_id <Any-Enter> \
            [code $canvas_ configure -cursor $itk_option(-linecursor)]
         $canvas_ bind $sel_id <Any-Leave> \
            [code $this reset_cursor]
         $canvas_ bind $sel_id <1> \
            [code eval $this mark $canvasX_ $canvasY_]
         $canvas_ bind $sel_id <Shift-1> \
            [code eval $this mark $canvasX_ $canvasY_]
         $canvas_ bind $sel_id <ButtonRelease-1> \
            [code $this end_resize_arc $id]
         $canvas_ bind $sel_id <Shift-ButtonRelease-1> \
            [code $this end_resize_arc $id]
         $canvas_ bind $sel_id <B1-Motion> \
            [code eval $this resize_arc $id $canvasX_ $canvasY_ $side]
         $canvas_ bind $sel_id <Shift-B1-Motion> \
            [code eval $this resize_arc $id $canvasX_ $canvasY_ $side]
      }
      adjust_arc_selection $id
   }

   #  Draw selection grips for a point editable polygon.
   method draw_pointpoly_selection_grips {id} {
      for {set i 0} {$i < $obj_points_} {incr i} {
         set sel_id [$canvas_ create rectangle 0 0 \
                        $itk_option(-gripwidth) $itk_option(-gripwidth) \
                        -tags "grip grip.$id grip.$id.$i" \
                        -fill $itk_option(-gripfill) \
                        -outline $itk_option(-gripoutline)]
         $canvas_ bind $sel_id <Any-Enter> \
            [code $canvas_ configure -cursor $itk_option(-linecursor)]
         $canvas_ bind $sel_id <Any-Leave> \
            [code $this reset_cursor]
         $canvas_ bind $sel_id <1> \
            [code eval $this mark $canvasX_ $canvasY_]
         $canvas_ bind $sel_id <Shift-1> \
            [code eval $this mark $canvasX_ $canvasY_]
         $canvas_ bind $sel_id <ButtonRelease-1> \
            [code $this end_move_pointpoly $id]
         $canvas_ bind $sel_id <Shift-ButtonRelease-1> \
            [code $this end_move_pointpoly $id]
         $canvas_ bind $sel_id <B1-Motion> \
            [code eval $this move_pointpoly $id $canvasX_ $canvasY_ $i]
         $canvas_ bind $sel_id <Shift-B1-Motion> \
            [code eval $this move_pointpoly $id $canvasX_ $canvasY_ $i]
         $canvas_ bind $sel_id <2> \
            [code eval $this insert_pointpoly $id $canvasX_ $canvasY_ $i]
         $canvas_ bind $sel_id <3> \
            [code eval $this remove_pointpoly $id $canvasX_ $canvasY_ $i]
      }
      adjust_pointpoly_selection $id
   }

   #  Deselect the given object.

   method deselect_object {id} {
      util::CanvasDraw::deselect_object $id
      if {[info exists notify_selected_($id)]} {
         eval "$notify_selected_($id) deselected"
      }
   }

   #  Deselect all canvas objects

   method deselect_objects {} {
      foreach id [selected_items] {
         if {[info exists notify_selected_($id)]} {
            eval "$notify_selected_($id) deselected"
         }
      }
      util::CanvasDraw::deselect_objects
   }

   #  Adjust the selection handles for the given object to fit the new size/pos.

   method adjust_object_selection {id} {
      if {"[$canvas_ type $id]" == "rtd_ellipse" } {
         if { "$types_($id)" == "circle" } {
            adjust_circle_selection $id
         } else {
            adjust_ellipse_selection $id
         }
      } elseif {"[$canvas_ type $id]" == "arc" } {
         adjust_arc_selection $id
      } elseif {"[$canvas_ type $id]" == "rtd_rotbox" } {
         adjust_ellipse_selection $id
      } elseif { [info exists types_($id)] && $types_($id) == "pointpoly" } {
         adjust_pointpoly_selection $id
      } else {
         util::CanvasDraw::adjust_object_selection $id
      }
   }

   #  Adjust the selection handles for the given ellipse.

   method adjust_ellipse_selection {id} {
      set majx [$canvas_ itemcget $id -semimajorx]
      set majy [$canvas_ itemcget $id -semimajory]
      set minx [$canvas_ itemcget $id -semiminorx]
      set miny [$canvas_ itemcget $id -semiminory]
      set w [expr $itk_option(-gripwidth)/2]
      foreach i [list "e $majx $majy" "n $minx $miny"] {
         lassign $i side x y
         $canvas_ coords grip.$id.$side \
            [expr $x-$w] [expr $y-$w] \
            [expr $x+$w] [expr $y+$w]
      }
   }

   #  Resize an ellipse.

   method resize_ellipse {id x y side} {
      set resizing_ 1
      if {"$side" == "e" } {
         $canvas_ itemconfigure $id -semimajorx $x -semimajory $y
      } else {
         $canvas_ itemconfigure $id -semiminorx $x -semiminory $y
      }
      if {[info exists notify_update_($id)]} {
         eval "$notify_update_($id) resize"
      }
   }

   #  Stop resizing the selected ellipse.

   method end_resize_ellipse {id} {
      if {[info exists notify_($id)]} {
         eval "$notify_($id) resize"
      }
      adjust_ellipse_selection $id
      set resizing_ 0
      reset_cursor
   }

   #  Adjust the selection handles for the given circle.

   method adjust_circle_selection {id} {
      lassign [$canvas_ coords $id] x y
      set smaj [$canvas_ itemcget $id -semimajor]
      set x [expr $x+$smaj]
      set w [expr $itk_option(-gripwidth)/2]
      $canvas_ coords grip.$id [expr $x-$w] [expr $y-$w] \
         [expr $x+$w] [expr $y+$w]
   }

   #  Resize a circle.

   method resize_circle {id x y} {
      set resizing_ 1
      lassign [$canvas_ coords $id] x0 y0
      set dx [expr ($x0-$x)]
      set dy [expr ($y0-$y)]
      set smaj [expr sqrt($dx*$dx+$dy*$dy)]
      $canvas_ itemconfigure $id -semimajor $smaj -semiminor $smaj -angle 0

      if {[info exists notify_update_($id)]} {
         eval "$notify_update_($id) resize"
      }
   }

   #  Stop resizing the selected circle.

   method end_resize_circle {id} {
      if {[info exists notify_($id)]} {
         eval "$notify_($id) resize"
      }
      adjust_circle_selection $id
      set resizing_ 0
      reset_cursor
   }

   #  Adjust the selection handles for the given square.

   method adjust_square_selection {id} {
      lassign [$canvas_ coords $id] x0 y0 x1 y1
      set ymid [expr ($y0+$y1)/2]
      set w [expr $itk_option(-gripwidth)/2]
      $canvas_ coords grip.$id [expr $x1-$w] [expr $ymid-$w] \
         [expr $x1+$w] [expr $ymid+$w]
   }

   #  Resize a square

   method resize_square {id x y} {
      set resizing_ 1
      lassign [$canvas_ coords $id] x0 y0 x1 y1
      set centx [expr ($x0+$x1)/2]
      set centy [expr ($y0+$y1)/2]
      set hwid [expr $x-$centx > $y-$centy ? $x-$centx : $y-$centy]
      $canvas_ coords $obj_id_ [expr $centx-$hwid] [expr $centy-$hwid] \
               [expr $centx+$hwid] [expr $centy+$hwid]

      if {[info exists notify_update_($id)]} {
         eval "$notify_update_($id) resize"
      }
   }

   #  Stop resizing the selected square

   method end_resize_square {id} {
      if {[info exists notify_($id)]} {
         eval "$notify_($id) resize"
      }
      adjust_square_selection $id
      set resizing_ 0
      reset_cursor
   }

   #  Adjust the selection handles for the given arc

   method adjust_arc_selection {id} {
      # Place a grip in the middle of the arc and one near the end of
      # the -start radius
      lassign [$canvas_ coords $id] x0 y0 x1 y1
      # pa and oa in radians
      set pa [expr [$canvas_ itemcget $id -start]/$rad_to_deg_]
      set oa [expr [$canvas_ itemcget $id -extent]/$rad_to_deg_]
      set centx [expr ($x0+$x1)/2]
      set centy [expr ($y0+$y1)/2]
      set rad [expr ($x1-$x0)/2]
      # remember y increases downwards
      set arcx  [expr $centx+$rad*cos($pa+$oa/2)]
      set arcy  [expr $centy-$rad*sin($pa+$oa/2)]
      set linex [expr $centx+0.9*$rad*cos($pa)]
      set liney [expr $centy-0.9*$rad*sin($pa)]

      set w [expr $itk_option(-gripwidth)/2]
      foreach i [list "arc $arcx $arcy" "line $linex $liney"] {
         lassign $i side x y
         $canvas_ coords grip.$id.$side \
               [expr $x-$w] [expr $y-$w] \
               [expr $x+$w] [expr $y+$w]
      }
   }

   #  Resize a arc.

   method resize_arc {id x y side} {
      set resizing_ 1

      lassign [$canvas_ coords $id] x0 y0 x1 y1

      set centx [expr ($x0+$x1)/2]
      set centy [expr ($y0+$y1)/2]
      set dx [expr $x-$centx]
      set dy [expr $y-$centy]

      # ang in degrees
      # Remember y increases downwards,
      # and that atan2(y,x) is arctan(y/x) in correct quadrant
      # (the tcl book is wrong)
      set ang [expr atan2(-$dy,$dx) * $rad_to_deg_]

      set oa [$canvas_ itemcget $id -extent]
      if {$side == "arc"} {
         # oa in degrees
         set rad [expr sqrt($dx*$dx+$dy*$dy)]
         $canvas_ coords $id \
               [expr $centx-$rad] [expr $centy-$rad] \
               [expr $centx+$rad] [expr $centy+$rad]
         $canvas_ itemconfigure $id -start [expr $ang-($oa/2)]
      } else {
         # side=line
         set rad [expr sqrt($dx*$dx+$dy*$dy)/0.9]
         $canvas_ coords $id \
               [expr $centx-$rad] [expr $centy-$rad] \
               [expr $centx+$rad] [expr $centy+$rad]
         # pa in degrees
         set pa [$canvas_ itemcget $id -start]
         $canvas_ itemconfigure $id \
               -extent [expr abs($oa+2*($pa-$ang))] -start $ang
      }
      if {[info exists notify_update_($id)]} {
         eval "$notify_update_($id) resize"
      }
   }

   #  Stop resizing the selected arc.

   method end_resize_arc {id} {
      if {[info exists notify_($id)]} {
         eval "$notify_($id) resize"
      }
      adjust_arc_selection $id
      set resizing_ 0
      reset_cursor
   }

   #  Adjust the selection handles for the given pointpoly. Note that
   #  update to canvas position needs to remove the extra point returned
   #  by the canvas command (this is the repeat of point 1).

   method adjust_pointpoly_selection {id} {
      if { $obj_points_ > 3 } {
         set limit [expr ($obj_points_*2)-1]
      } else {
         set limit 5
      }
      set obj_coords_ [lrange [$canvas_ coords $id] 0 $limit]
      set w [expr $itk_option(-gripwidth)/2]
      set index 0
      for {set i 0} {$i < $obj_points_} {incr i} {
         set x [lindex $obj_coords_ $index]
         incr index
         set y [lindex $obj_coords_ $index]
         incr index
         $canvas_ coords grip.$id.$i \
            [expr $x-$w] [expr $y-$w] \
            [expr $x+$w] [expr $y+$w]
      }
   }

   #  Move a point in a polygon.

   method move_pointpoly {id x y npair} {
      set resizing_ 1
      set index [expr $npair*2]
      set obj_coords_ [lreplace $obj_coords_ $index [expr $index+1] $x $y]
      eval $canvas_ coords $id $obj_coords_
      if {[info exists notify_update_($id)]} {
         eval "$notify_update_($id) move"
      }
   }

   #  Stop moving the polygon point.

   method end_move_pointpoly {id} {
      if {[info exists notify_($id)]} {
         eval "$notify_($id) move"
      }
      adjust_pointpoly_selection $id
      set resizing_ 0
      reset_cursor
   }

   #  Create a new object in the canvas. Note this is a copy of the
   #  function as used in CanvasDraw (seems to need this to allow the
   #  types_ array to access obj_id_).

   method create_object {x y} {
      mark $x $y
      set obj_coords_ "$x $y"
      set obj_id_ [create_$drawing_mode_ $x $y]
      if {"$drawing_mode_" != "region"} {
         $canvas_ addtag objects withtag $obj_id_
         $canvas_ addtag $w_.objects withtag $obj_id_
         set types_($obj_id_) $drawing_mode_
      }
      set resizing_ 1
   }

   #  Override add_object_bindings to deal with the case when a canvas
   #  item is created directly (in which case we do not know the id).
   method add_object_bindings {id {target_id ""} } {
      util::CanvasDraw::add_object_bindings $id $target_id
      if { ! [info exists types_($id)] } {

         #  This is an item we don't know about. Add it to the lists.
         $canvas_ addtag objects withtag $id
         $canvas_ addtag $w_.objects withtag $id
         set types_($id) $drawing_mode_
         if { [$canvas_ type $id] == "rtd_ellipse" } {
            set types_($id) ellipse
         } elseif { [$canvas_ type $id] == "rtd_rotbox" } {
            set types_($id) rotbox
         }
      }
   }

   #  Create and return a new circle object.

   method create_circle {x y} {
      return [$canvas_ create rtd_ellipse $x $y \
                 -width $itk_option(-linewidth) \
                 -fill $itk_option(-fillcolor) \
                 -outline $itk_option(-outlinecolor) \
                 -stipple $itk_option(-stipplepattern)]
   }

   #  Create and return a new ellipse object

   method create_ellipse {x y} {
      return [$canvas_ create rtd_ellipse $x $y \
                 -width $itk_option(-linewidth) \
                 -fill $itk_option(-fillcolor) \
                 -outline $itk_option(-outlinecolor) \
                 -stipple $itk_option(-stipplepattern)]
   }

   #  Create and return a square object

   method create_square {x y} {
       return [$canvas_ create rectangle $x $y $x $y \
                 -fill $itk_option(-fillcolor) \
                 -outline $itk_option(-outlinecolor) \
                 -width $itk_option(-linewidth) \
                 -stipple $itk_option(-stipplepattern)]
   }

   #  Create and return an xrange object, use a segmented line.

   method create_xrange {x y} {
      return [$canvas_ create rtd_segment $x $y $x $y $x $y $x $y $x $y $x $y \
                 -width $itk_option(-linewidth) \
                 -fill [get_line_color] \
                 -stipple $itk_option(-linestipple) \
                 -arrow $itk_option(-arrowtype) \
                 -smooth $itk_option(-smooth) \
                 -arrowshape $itk_option(-arrowshape)]
   }

   #  Create and return a yrange object, use a segmented line.

   method create_yrange {x y} {
      return [$canvas_ create rtd_segment $x $y $x $y $x $y $x $y $x $y $x $y \
                 -width $itk_option(-linewidth) \
                 -fill [get_line_color] \
                 -stipple $itk_option(-linestipple) \
                 -arrow $itk_option(-arrowtype) \
                 -smooth $itk_option(-smooth) \
                 -arrowshape $itk_option(-arrowshape)]
   }

   #  Create and return a arc object

   method create_arc {x y} {
      return [$canvas_ create arc $x $y $x $y \
                 -start 90 \
                 -extent 90 \
                 -style pieslice \
                 -width $itk_option(-linewidth) \
                 -fill $itk_option(-fillcolor) \
                 -stipple $itk_option(-stipplepattern) \
                 -outline $itk_option(-outlinecolor)]
   }
   method update_arc {x y} {
      lassign [$canvas_ coords $obj_id_] x0 y0 x1 y1
      set oa [$canvas_ itemcget $obj_id_ -extent]
      set centx [expr ($x0+$x1)/2]
      set centy [expr ($y0+$y1)/2]
      set dx [expr $x-$centx]
      set dy [expr $y-$centy]
      set rad [expr sqrt($dx*$dx+$dy*$dy)]

      # remember y increases downwards
      set newang [expr atan2(-$dy,$dx) * $rad_to_deg_]

      $canvas_ coords $obj_id_ \
            [expr $centx-$rad] [expr $centy-$rad] \
            [expr $centx+$rad] [expr $centy+$rad]
      $canvas_ itemconfigure $obj_id_ -start [expr $newang-($oa/2)]
   }

   # Return arc coordinates corresponding to id, as a list, or {} if
   # no such coordinates.
   # Return value is a list, of [x y r pa oa].  (x,y,r) are the position of
   # the centre of the circle of which this is a arc, and its
   # radius.  (pa) is the position-angle of the centre-line of the
   # arc, in degrees from the x-axis, measured anti-clockwise.
   # (oa) is the opening-angle of the arc, in degrees, from radius
   # to radius (ie, not from radius to centre-line)
   public method arc_coords {id} {
      lassign [$canvas_ coords $id] x0 y0 x1 y1
      # pa and oa in degrees
      set pa [$canvas_ itemcget $id -start]
      set oa [$canvas_ itemcget $id -extent]

      set centx [expr ($x0+$x1)/2]
      set centy [expr ($y0+$y1)/2]

      return [list $centx $centy [expr ($x1-$x0)/2] [expr $pa+$oa/2] $oa]
   }

   #  Create and return a pointpoly object

   method create_pointpoly {x y} {
      set obj_coords_ "$x $y $x $y $x $y"
      set obj_points_ 0
      return [$canvas_ create polygon $x $y $x $y $x $y  \
                 -fill $itk_option(-fillcolor) \
                 -outline $itk_option(-outlinecolor) \
                 -width $itk_option(-linewidth) \
                 -stipple $itk_option(-stipplepattern)]
   }

   #  Create and return a new rotbox object

   method create_rotbox {x y} {
      return [$canvas_ create rtd_rotbox $x $y \
                 -width $itk_option(-linewidth) \
                 -fill $itk_option(-fillcolor) \
                 -outline $itk_option(-outlinecolor) \
                 -stipple $itk_option(-stipplepattern)]
   }

   #  Create and return a new column object.

   method create_column {x y} {
      set height [$itk_option(-rtdimage) dispheight]
      set id [$canvas_ create line $x 0 $x $height \
                 -width $pixel_width_ \
                 -fill $itk_option(-outlinecolor) \
                 -stipple $itk_option(-linestipple)]
      $canvas_ addtag pixel_width_objects withtag $id
      return $id
   }

   #  Create and return a new row object.

   method create_row {x y} {
      set width [$itk_option(-rtdimage) dispwidth]
      set id [$canvas_ create line 0 $y $width $y \
                 -width $pixel_width_ \
                 -fill $itk_option(-outlinecolor) \
                 -stipple $itk_option(-linestipple)]
      $canvas_ addtag pixel_width_objects withtag $id
      return $id
   }

   #  Create and return a new pixel object.

   method create_pixel {x y} {
      set id [$canvas_ create rectangle $x $y $x $y \
                 -width $pixel_width_ \
                 -fill $itk_option(-fillcolor) \
                 -outline $itk_option(-outlinecolor) \
                 -stipple $itk_option(-stipplepattern)]
      $canvas_ addtag pixel_width_objects withtag $id
      return $id
   }


   #  Update ellipse.

   method update_ellipse {x y} {
      $canvas_ itemconfigure $obj_id_ -semimajorx $x -semimajory $y
   }

   #  Update circle.

   method update_circle {x y} {
      lassign [$canvas_ coords $obj_id_] x0 y0
      set dx [expr ($x0-$x)]
      set dy [expr ($y0-$y)]
      set smaj [expr sqrt($dx*$dx+$dy*$dy)]
      $canvas_ itemconfigure $obj_id_ -semimajor $smaj -semiminor $smaj -angle 0
   }

   #  Add a point to the current polygon. Only do this if have set the
   #  positions of the first three points (these are formed on
   #  creation of the object, but have no initial position). If we are
   #  dealing with the first three points and are updating the second
   #  position, then the third is modified to the same value as the
   #  second. This makes the moving update look correct.
   method add_pointpoly_point {x y {interactive 1}} {
      if { $obj_points_ < 3 } {
         set index [expr $obj_points_*2]
         set obj_coords_ [lreplace $obj_coords_ $index [expr $index+1] $x $y]
         if { $obj_points_ == 1 } {
            set obj_coords_ [lreplace $obj_coords_ 4 5 $x $y]
         }
      } else {
         append obj_coords_ " $x $y"
      }
      if { $interactive } {
         bind $canvas_ <1> {}
         bind $canvas_ <Motion> \
            [code eval $this update_pointpoly $canvasX_ $canvasY_]
      }
      update_pointpoly $x $y
      incr obj_points_
   }

   #  Update a pointpoly object.

   method update_pointpoly {x y} {
      eval $canvas_ coords $obj_id_ $obj_coords_ $x $y
   }

   #  Insert a new point in a polygon. Note this is offset slightly as
   #  it is expected to be invoked by a grip binding.
   method insert_pointpoly {id x y npair} {
      set index [expr $npair*2]
      set x [expr $x+5.0]
      set y [expr $y+5.0]
      set obj_coords_ [linsert $obj_coords_ $index $x $y]
      incr obj_points_
      update_pointpoly $x $y
      eval $canvas_ coords $id $obj_coords_
      deselect_object $id
      select_object $id
      if {[info exists notify_update_($id)]} {
         eval "$notify_update_($id) move"
      }
   }

   #  Remove a point from the polygon.
   method remove_pointpoly {id x y npair} {
      if { $obj_points_ > 3 } {
         set index [expr $npair*2]
         set obj_coords_ [join [lreplace $obj_coords_ $index [incr index] "" ""]]
         incr obj_points_ -1
         eval $canvas_ coords $id $obj_coords_
         deselect_object $id
         select_object $id
         if {[info exists notify_update_($id)]} {
            eval "$notify_update_($id) move"
         }
      }
   }

   #  Update column to new position (obj_id_ not known when setting bindings
   #  for column creation, hence need this method as well as move_object).

   method update_column {x y} {
      lassign [$canvas_ coords $obj_id_] x0 y0 x1 y1
      $canvas_ coords $obj_id_ $x $y0 $x $y1
   }

   #  Update row to new position (obj_id_ not known when setting bindings
   #  for row creation, hence need this method as well as move_object).

   method update_row {x y} {
      lassign [$canvas_ coords $obj_id_] x0 y0 x1 y1
      $canvas_ coords $obj_id_ $x0 $y $x1 $y
   }

   #  Update square.
   method update_square {x y} {
       lassign [$canvas_ coords $obj_id_] x0 y0 x1 y1
       set centx [expr ($x0+$x1)/2]
       set centy [expr ($y0+$y1)/2]
       set hwid [expr $x-$centx > $y-$centy ? $x-$centx : $y-$centy]
       $canvas_ coords $obj_id_ [expr $centx-$hwid] [expr $centy-$hwid] \
               [expr $centx+$hwid] [expr $centy+$hwid]
   }

   #  Update xrange.
   method update_xrange {x y} {
      lassign [$canvas_ bbox $obj_id_] x0 y0 x1 y1
      set centy [expr ($y0+$y1)/2]
      $canvas_ coords $obj_id_ \
         $x0 $y0 $x0 $y1 $x0 $centy $x1 $centy $x1 $y0 $x1 $y1
   }

   #  Update yrange.
   method update_yrange {x y} {
      lassign [$canvas_ bbox $obj_id_] x0 y0 x1 y1
      set centx [expr ($x0+$x1)/2]
      $canvas_ coords $obj_id_ \
         $x0 $y0 $x1 $y0 $centx $y0 $centx $y1 $x0 $y1 $x1 $y1
   }

   #  Update pixel to new position (obj_id_ not known when setting bindings
   #  for pixel creation, hence need this method as well as move_object).

   method update_pixel {x y} {
      $canvas_ coords $obj_id_ $x $y $x $y
   }

   #  Arrange to have cmd evaluated whenever the given object is
   #  selected or deselected.
   method add_selected_notify {id cmd} {
      set notify_selected_($id) $cmd
   }

   #  Remove the notify selected/deselected command.
   method remove_selected_notify {id} {
      if { [info exists notify_selected_($id)] } {
         unset notify_selected_($id)
      }
   }

   #  Method to change the width of any items that need to scale with
   #  the RTD image.
   method pixel_width_changed {} {
      update_pixel_width
      foreach i [$canvas_ find withtag pixel_width_objects] {
         $canvas_ itemconfigure $i -width $pixel_width_
      }
   }

   #  Return the number of points in a point adjustable object. Note
   #  this should be used instead of the canvas coords mechanism as extra
   #  points may be appended.
   method obj_points {} {
      return $obj_points_
   }

   #  Take control of rotation (interchange) and flipping so that we
   #  can honour the new -ignore_tag option.

   #  Rotate the named item(s) 90 degrees by swapping the X and Y coords.
   method rotate {item} {
      set ids [$canvas_ find withtag $item]
      if { $itk_option(-ignore_tag) == {} } {
         foreach id $ids {
            catch {
               set coords [$canvas_ coords $id]
               set n [llength $coords]
               set ncoords {}
               for {set i 0} {$i < $n} {incr i 2} {
                  lappend ncoords [lindex $coords [expr $i+1]] [lindex $coords $i]
               }
               eval $canvas_ coords $id $ncoords
            }
         }
      } else {
         foreach id $ids {
            if {! [string match "*$itk_option(-ignore_tag)*" [$canvas_ gettags $id]]} {
               catch {
                  set coords [$canvas_ coords $id]
                  set n [llength $coords]
                  set ncoords {}
                  for {set i 0} {$i < $n} {incr i 2} {
                     lappend ncoords [lindex $coords [expr $i+1]] [lindex $coords $i]
                  }
                  eval $canvas_ coords $id $ncoords
               }
            }
         }
      }
   }

   #  Flip the named item(s) on the X axis by subtracting the x
   #  coordinates from $maxx.
   method flipx {item maxx} {
      set ids [$canvas_ find withtag $item]
      if { $itk_option(-ignore_tag) == {} } {
         foreach id $ids {
            catch {
               set coords [$canvas_ coords $id]
               set n [llength $coords]
               set ncoords {}
               for {set i 0} {$i < $n} {incr i 2} {
                  lappend ncoords [expr $maxx-[lindex $coords $i]] \
                     [lindex $coords [expr $i+1]]
               }
               eval $canvas_ coords $id $ncoords
            }
         }
      } else {
         foreach id $ids {
            if {! [string match "*$itk_option(-ignore_tag)*" [$canvas_ gettags $id]]} {
               catch {
                  set coords [$canvas_ coords $id]
                  set n [llength $coords]
                  set ncoords {}
                  for {set i 0} {$i < $n} {incr i 2} {
                     lappend ncoords [expr $maxx-[lindex $coords $i]] \
                        [lindex $coords [expr $i+1]]
                  }
                  eval $canvas_ coords $id $ncoords
               }
            }
         }
      }
   }

   #  Flip the named item(s) on the Y axis by subtracting the y
   #  coordinates from $maxy.
   method flipy {item maxy} {
      set ids [$canvas_ find withtag $item]
      if { $itk_option(-ignore_tag) == {} } {
         foreach id $ids {
            catch {
               set coords [$canvas_ coords $id]
               set n [llength $coords]
               set ncoords {}
               for {set i 0} {$i < $n} {incr i 2} {
                  lappend ncoords [lindex $coords $i] [expr $maxy-[lindex $coords [expr $i+1]]]
               }
               eval $canvas_ coords $id $ncoords
            }
         }
      } else {
         foreach id $ids {
            if {! [string match "*$itk_option(-ignore_tag)*" [$canvas_ gettags $id]]} {
               catch {
                  set coords [$canvas_ coords $id]
                  set n [llength $coords]
                  set ncoords {}
                  for {set i 0} {$i < $n} {incr i 2} {
                     lappend ncoords [lindex $coords $i] [expr $maxy-[lindex $coords [expr $i+1]]]
                  }
                  eval $canvas_ coords $id $ncoords
               }
            }
         }
      }
   }

   #  Redefine clip method to inhibit this action.
   method clip {x y} {
   }

   #  Redefine select only the given object to update the pointpoly
   #  when required (trap for only moving mouse stopped this at v2.0.7).
   method select_only_object {id x y} {
      util::CanvasDraw::select_only_object $id $x $y
      if { [info exists types_($id)] && $types_($id) == "pointpoly" } {
         adjust_pointpoly_selection $id
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of RTD image displayed on canvas. This is used to define
   #  the width of certain items (column, row and pixel).
   itk_option define -rtdimage rtdimage RtdImage {} {}

   #  Item on the canvas below which other should be be lowered.
   itk_option define -lowestitem lowestitem LowestItem {} {}

   #  A canvas tag to ignore when flipping or rotating items.
   itk_option define -ignore_tag ignore_tag Ignore_Tag {} {}

   #  A size for the button at the centre of a arc
   itk_option define -arcbutton arcbutton Arcbutton {} {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  List of drawing types available.
   protected variable drawing_modes_ {
      anyselect objselect line rectangle oval circle polygon polyline
      text ellipse column row pixel rotbox pointpoly square arc xrange yrange
   }

   #  Width of an image pixel.
   protected variable pixel_width_ 1

   #  Command to be evaluated when the item is selected or deselected,
   #  the return has the string "selected" or "deselected" appended,
   protected variable notify_selected_

   #  Types of object (indexed by id).
   protected variable types_

   #  Number of points in point adjustable object.
   protected variable obj_points_ 0

   #  Private variables (available only to the instance)
   #  -----------------


   #  Common variables: (shared by all instances)
   #  -----------------

   #common pi_ 3.141592657
   common rad_to_deg_ 57.295779450887607

#  End of class definition.
}


#+
#  Name:
#     Gaia3dVtkLine

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a line prop.

#  Description:
#     Class to create instances of a vtkLineSource and provide
#     methods to manage the related work required to render it
#     in a scene and provide the ability to fix it to the bounds
#     and axis of a vtkImageData instance (cube).

#  Copyright:
#     Copyright (C) 2007 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     07-AUG-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkLine {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the line source.
      set line_ [::vtkLineSource New]
      $line_ SetResolution 5

      #  Create the mapper.
      set mapper_ [::vtkPolyDataMapper New]
      $mapper_ SetInput [$line_ GetOutput]

      #  And the prop.
      set prop_ [::vtkActor New]
      $prop_ SetMapper $mapper_

      #  Set some properties.
      set property_ [::vtkProperty New]
      $property_ SetRepresentationToSurface
      #$property_ SetColor 0.0 1.0 1.0
      $property_ SetAmbient 1.0
      $property_ SetAmbientColor 1.0 1.0 1.0
      $property_ SetLineWidth 2.0
      $prop_ SetProperty $property_

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      remove_from_window
      $prop_ Delete
      $mapper_ Delete
      $line_ Delete
      $property_ Delete
   }

   #  Methods and procedures:
   #  -----------------------

   #  Add this line to the render window.
   public method add_to_window {} {
      $renwindow add_view_prop $prop_
   }

   #  Remove this line from a render window.
   public method remove_from_window {} {
      $renwindow remove_view_prop $prop_
   }

   #  Make line visible.
   public method set_visible {} {
      $prop_ VisibilityOn
      $renwindow modified
   }

   #  Make line invisible.
   public method set_invisible {} {
      $prop_ VisibilityOff
      $renwindow modified
   }

   #  Set the positions of the endpoints.
   public method set_end_points {x1 y1 z1 x2 y2 z2} {
      set x1_ $x1
      set y1_ $y1
      set z1_ $z1
      set x2_ $x2
      set y2_ $y2
      set z2_ $z2

      if { $clip_to_bounds } {
         lassign [clip_position_ $x1_ $y1_ $z1_] x1_ y1_ z1_
         lassign [clip_position_ $x2_ $y2_ $z2_] x2_ y2_ z2_
      }
      axis_align_
      update_end_points_
   }

   #  When axis aligned should be able to just set the two positions on the
   #  image plane that define the axis. If not axis aligning do nothing.
   #  Need to have called set_end_points at least once. Note one of p1,
   #  p2 or p3 will be ignored (just avoids needed to pick these from a
   #  general position).
   public method set_position {p1 p2 p3} {
      if { $align_to_axis } {
         if { $axis == 1 } {
            set y1_ $p2
            set y2_ $p2
            set z1_ $p3
            set z2_ $p3
         } elseif { $axis == 2 } {
            set x1_ $p1
            set x2_ $p1
            set z1_ $p3
            set z2_ $p3
         } else {
            set x1_ $p1
            set x2_ $p1
            set y1_ $p2
            set y2_ $p2
         }
         update_end_points_
      }
   }

   #  When axis aligned should be able to just set the two positions on the
   #  image plane that define the axis. If not axis aligning do nothing.
   #  Need to have called set_end_points at least once. In this version
   #  p1 and p2 are the position on the image plane normal to the axis.
   public method set_axis_position {p1 p2} {
      if { $align_to_axis } {
         if { $axis == 1 } {
            set y1_ $p1
            set y2_ $p1
            set z1_ $p2
            set z2_ $p2
         } elseif { $axis == 2 } {
            set x1_ $p1
            set x2_ $p1
            set z1_ $p2
            set z2_ $p2
         } else {
            set x1_ $p1
            set x2_ $p1
            set y1_ $p2
            set y2_ $p2
         }
         update_end_points_
      }
   }

   #  When axis aligned return the position of the line in the image plane.
   #  Works when not axis aligned, but the position will be incorrect.
   public method get_axis_position {} {
      if { $axis == 1 } {
         return [list $y1_ $z1_]
      } elseif { $axis == 2 } {
         return [list $x1_ $z1_]
      }
      return [list $x1_ $y1_]
   }

   #  Make the line fit to the data cube. That is give it fake limits that
   #  position it within the cube in the right direction.
   public method fit_to_data {} {
      lassign [get_dimensions_] xdim ydim zdim
      if { $axis == 1 } {
         set_end_points 1 $y1_ $z1_ $xdim $y1_ $z1_
      } elseif { $axis == 2 } {
         set_end_points $x1_ 1 $z1_ $x1_ $ydim $z1_
      } else {
         set_end_points $x1_ $y1_ 1 $x1_ $y1_ $zdim
      }
   }

   #  Update the end points to the current values.
   protected method update_end_points_ {} {
      $line_ SetPoint1 $x1_ $y1_ $z1_
      $line_ SetPoint2 $x2_ $y2_ $z2_
      $line_ Update
   }

   #  Clip a position to lie with the current cube. If no cube is defined
   #  then the position is returned unchanged.
   protected method clip_position_ {x y z} {
      if { $dataset != {} } {
         lassign [get_dimensions_] xdim ydim zdim
         if { $x < 0 } {
            set x 0
         } elseif { $x > $xdim } {
            set x $xdim
         }
         if { $y < 0 } {
            set y 0
         } elseif { $y > $ydim } {
            set y $ydim
         }
         if { $z < 0 } {
            set z 0
         } elseif { $z > $zdim } {
            set z $zdim
         }
      }
      return [list $x $y $z]
   }

   #  Change the current end points so that they align to the current axis.
   #  The first end point position is dominant.
   protected method axis_align_ {} {
      if { $align_to_axis } {
         if { $axis == 1 } {
            set y2_ $y1_
            set z2_ $z1_
         } elseif { $axis == 2 } {
            set x2_ $x1_
            set z2_ $z1_
         } else {
            set x2_ $x1_
            set y2_ $y1_
         }
      }
   }

   #  Get the dimensions of the dataset (cube).
   protected method get_dimensions_ {} {
      if { $dataset != {} } {
         return [$dataset GetDimensions]
      }
      return [list 1 1 1]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The render window (a Gaia3dVtkWindow instance).
   public variable renwindow {}

   #  The current vtkImageData instance. Make sure extent information is
   #  available.
   public variable dataset {} {
      if { $dataset != {} } {
         $dataset Update
      }
   }

   #  Whether to keep line within dataset bounds.
   public variable clip_to_bounds 1

   #  Whether to align to an axis.
   public variable align_to_axis 0

   #  The axis to align to.
   public variable axis 3

   #  The colour (Tcl colour).
   public variable colour {#0ff} {
      if { $property_ != {} } {
         set rgb [::gaia3d::Gaia3dVtkWindow::get_rgb_colour $colour]
         eval $property_ SetColor $rgb
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Current end points.
   protected variable x1_ 0
   protected variable y1_ 0
   protected variable z1_ 0
   protected variable x2_ 0
   protected variable y2_ 0
   protected variable z2_ 0

   #  VTK objects.
   protected variable line_ {}
   protected variable mapper_ {}
   protected variable prop_ {}
   protected variable property_

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

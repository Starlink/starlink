#+
#  Name:
#     Gaia3dVtkPrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Base class for creating and manipulating prisms.

#  Description:
#     Base class to create instances of a vtkPolyData that represent the
#     extrusion of a 2D shape into a third dimension and provide methods to
#     manage the related work required to render it in a scene and provide the
#     ability to fix to the given bounds or axis of a vtkImageData instance
#     (cube). This is extended for specific shapes by implementing a
#     create_polygon_ method that describes the 2D locus of the shape.

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council
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
#     27-APR-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkPrism {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the polydata, points and cells that will describe the 2D
      #  locus.
      set polydata_ [::vtkPolyData New]
      set points_ [::vtkPoints New]
      set cells_ [::vtkCellArray New]

      #  Object to perform the extrusion.
      set extrude_ [::vtkLinearExtrusionFilter New]
      $extrude_ SetInput $polydata_
      $extrude_ SetExtrusionTypeToVectorExtrusion
      $extrude_ CappingOff

      #  Mapper.
      set mapper_ [::vtkPolyDataMapper New]
      $mapper_ SetInput [$extrude_ GetOutput]

      #  And the prop.
      set prop_ [::vtkActor New]
      $prop_ SetMapper $mapper_

      #  Set some properties.
      set property_ [::vtkProperty New]
      $property_ SetRepresentationToSurface
      $property_ SetColor 0.0 1.0 0.0
      $property_ SetOpacity 1.0
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
      $points_ Delete
      $cells_ Delete
      $polydata_ Delete
      $extrude_ Delete
      $property_ Delete
   }

   #  Methods:
   #  --------

   #  Create the 2D polygon representing the current shape. It is assumed
   #  that the shape is positioned at "zlow" and will extrude to "zhigh".
   #  For a cube "zlow" is the back or front face.
   protected method create_polygon_ {} {
      error "You need to implement a create_polygon_ method"
   }

   #  Apply a shift to the shape. Clearly can depends on the actual shape
   #  properties (parameterised shapes, like circles, will typically need
   #  regenerating), so must be implementation in a sub-class.
   protected method apply_shift_ {sx sy} {
      error "You need to implement a apply_shift_ method"
   }

   #  Shift the position of the shape by the given increments.
   #  Increments can be given for all axes and the correct pair will be used.
   public method shift_position {dx dy dz} {
      if { $axis == 1 } {
         set s1 $dy
         set s2 $dz
      } elseif { $axis == 2 } {
         set s1 $dx
         set s2 $dz
      } else {
         set s1 $dx
         set s2 $dy
      }
      apply_shift_ $s1 $s2
      update_
   }

   #  Add to the render window.
   public method add_to_window {} {
      $renwindow add_view_prop $prop_
   }

   #  Remove from the render window.
   public method remove_from_window {} {
      $renwindow remove_view_prop $prop_
   }

   #  Make visible.
   public method set_visible {} {
      $prop_ VisibilityOn
      $renwindow modified
   }

   #  Make invisible.
   public method set_invisible {} {
      $prop_ VisibilityOff
      $renwindow modified
   }

   #  Update object to use the current settings.
   protected method update_ {} {
      create_polygon_
      extrude_polygon_
   }

   #  Make the prism fit to the data cube or the given extent along the
   #  current axis. Call when ready to apply changes.
   public method fit_to_data {} {

      if { $dataset != {} } {
         #  Fitting to the data cube bounds.
         lassign [get_dimensions_] xdim ydim zdim
         if { $axis == 1 } {
            set length_ $xdim
         } elseif { $axis == 2 } {
            set length_ $ydim
         } else {
            set length_ $zdim
         }
      } else {
         #  Extruding to the given limits.
         set length_ [expr $zhigh-$zlow]
      }
      if { $axis == 1 } {
         set extrusion_vector_ {1 0 0}
      } elseif { $axis == 2 } {
         set extrusion_vector_ {0 1 0}
      } else {
         set extrusion_vector_ {0 0 1}
      }
      update_
   }

   #  Get the dimensions of the dataset (cube).
   protected method get_dimensions_ {} {
      if { $dataset != {} } {
         return [$dataset GetDimensions]
      }
      return [list 1 1 1]
   }

   #  Extrude the polygon so that it is projected to fill the data array
   #  extent in the direction of the selected axis.
   protected method extrude_polygon_ {} {
      eval $extrude_ SetVector $extrusion_vector_
      $extrude_ SetScaleFactor $length_
      $extrude_ SetInput $polydata_
      $polydata_ Modified
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

   #  The extrusion axis.
   public variable axis 3 {}

   #  The extrusion limits. Only used if the dataset is not defined.
   public variable zhigh 1
   public variable zlow 1

   #  The colour (Tcl colour).
   public variable colour {#0ff} {
      if { $property_ != {} } {
         set rgb [::gaia3d::Gaia3dVtkWindow::get_rgb_colour $colour]
         eval $property_ SetColor $rgb
      }
   }

   #  Align prism to axis. Provided to meet the Gaia3dArdPrismProxy
   #  interface. Has no effect as always aligned.
   public variable align_to_axis 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  VTK objects.
   protected variable extrude_ {}
   protected variable points_ {}
   protected variable cells_ {}
   protected variable polydata_ {}
   protected variable mapper_ {}
   protected variable prop_ {}
   protected variable property_

   #  Length of the selected axis.
   protected variable length_ 1

   #  Extraction normal vector.
   protected variable extrusion_vector_ {0 0 1}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Useful for parameterised shapes.
   common 2pi_ 6.28319
   common d2r_ 0.0174533
   common r2d_ 57.2958

#  End of class definition.
}

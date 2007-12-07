#+
#  Name:
#     Gaia3dVtkArdPrism

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Base class for creating and manipulating ARD prisms.

#  Description:
#     Base class to create instances of a vtkPolyData that represent the
#     extrusion of a 2D ARD shape into a third dimension and provide methods
#     to manage the related work required to render it in a scene and provide
#     the ability to fix it to the bounds and axis of a vtkImageData instance
#     (cube). This is extended for specific shapes by implementing a 
#     create_polygon_ method that describes the 2D locus of the shape.

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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     07-DEC-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkArdPrism {

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
      $property_ SetOpacity 0.8
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

   #  Methods and procedures:
   #  -----------------------

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

   #  Create the 2D polygon representing the ARD shape.
   protected method create_polygon_ {} {
      error "You need to implement a create_polygon_ method"
   }

   #  Make the prism fit to the data cube along the current axis.
   #  Call when ready to apply changes.
   public method fit_to_data {} {
      lassign [get_dimensions_] xdim ydim zdim
      if { $axis == 1 } {
         set length_ $xdim
         set extrusion_vector_ {1 0 0}
      } elseif { $axis == 2 } {
         set length_ $ydim
         set extrusion_vector_ {0 1 0}
      } else {
         set length_ $zdim
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
   common 2pi_ 6.28319
   common d2r_ 0.0174533
   common r2d_ 57.2958
   
#  End of class definition.
}

#+
#  Name:
#     Gaia3dVtkOutline

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Outline prop for cubes.

#  Description:
#     Class to create instances of a vtkOutlineFilter and provide
#     methods to manage the related work required to render it
#     in a scene and provide the ability to fix it to the bounds
#     of a vtkImageData instance (cube).

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
#     29-OCT-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkOutline {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the source.
      set source_ [::vtkOutlineSource New]

      #  Create the mapper.
      set mapper_ [::vtkPolyDataMapper New]
      $mapper_ SetInput [$source_ GetOutput]

      #  And the prop.
      set prop_ [::vtkActor New]
      $prop_ SetMapper $mapper_

      #  Set some properties.
      set property_ [::vtkProperty New]
      $property_ SetColor 0.0 0.0 0.0
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
      $source_ Delete
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

   #  Configuration options: (public variables)
   #  ----------------------

   #  The render window (a Gaia3dVtkWindow instance).
   public variable renwindow {}

   #  The current vtkImageData instance. Make sure extent information is
   #  available.
   public variable dataset {} {
      if { $dataset != {} } {
         $dataset Update
         set bounds [$dataset GetBounds]
         if { $bounds != {} } {
            eval $source_ SetBounds $bounds
         }
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  VTK objects.
   protected variable source_ {}
   protected variable mapper_ {}
   protected variable prop_ {}
   protected variable property_

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

#+
#  Name:
#     Gaia3dVtk

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class of VTK utility procedures.

#  Description:
#     Defines utility procedures with no connection to an obvious class.

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
#     03-JUL-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtk {

   #  Inheritances:
   #  -------------
   
   #  None

   #  Constructor:
   #  ------------
   private constructor {args} {
      package require vtk
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods and procedures:
   #  -----------------------

   #  Release all VTK objects. Should be called during application
   #  closure.
   public proc release_all {} {
      ::vtkCommand DeleteAllObjects
   }

   #  Switch debugging on or off.
   public proc debug { on } {
      if { $on } {
         ::vtkCommand DebugOn
      } else {
         ::vtkCommand DebugOff
      }
   }

   #  Switch global warnings on or off for all objects.
   public proc warnings { on } {
      if { $on } {
         ::vtkObject GlobalWarningDisplayOn
      } else {
         ::vtkObject GlobalWarningDisplayOff
      }
   }

   #  Show all instances of VTK Tcl objects.
   public proc show_all { {brief 1 } } {
      puts "VTK instances:"
      if { $brief } {
         foreach obj [::vtkCommand ListAllInstances] {
            puts "   $obj: [$obj GetClassName]"
         }
      } else {
         foreach obj [::vtkCommand ListAllInstances] {
            puts "   $obj: [$obj Print]"
         }
      }
   }

   #  Show all the methods of a VTK object.
   public proc show_methods {obj} {
      puts "[$obj ListMethods]"
   }

   #  Wrap an GaiaNDArray instance holding a cube to a vtkImageData
   #  structure.  Returns a vtkImageData instance containing the array data
   #  and the related vtkImageImport instance. These should be deleted
   #  together. 
   public proc import_gaia_cube { cubeaccessor checkbad nullvalue } {

      #  Get the dimensionality.
      set dims [$cubeaccessor getdims 0]
      set ndims [llength $dims]

      #  Make sure the cube data is available.
      set arrayinfo [$cubeaccessor map "READ" "DATA"]

      #  Make NDF data available in a vtkImageImport instance.
      set imageimport [::vtkImageImport New]
      set d1 [lindex $dims 0]
      set d2 [lindex $dims 1]
      set d3 [lindex $dims 2]
      incr d1 -1
      incr d2 -1
      incr d3 -1

      #  Dimensions.
      $imageimport SetWholeExtent 0 $d1 0 $d2 0 $d3
      $imageimport SetDataExtentToWholeExtent

      #  Do the sharing of the data from GAIA to VTK, if possible. 
      #  This normalises FITS data and returns if any bad values were 
      #  detected (strong truth), it can also replace any data values 
      #  with a null value (need to do this for volumes). If this happens
      #  then a copy of the data is made.
      set nobad [gvtk::setarray $arrayinfo $imageimport $checkbad $nullvalue]

      #  Read data into vktImageData.
      set imagedata [$imageimport GetOutput]

      #  Origins are 1, not 0 for GRID coordinates. We also have a default
      #  spacing of 1,1,1.
      $imagedata SetOrigin 1.0 1.0 1.0

      #  Want importdata to go away with the imagedata, so setup an observer.
      #  XXX doesn't work until application exit. Why?
      #$imagedata AddObserver DeleteEvent \
      #   [code gaia3d::Gaia3dVtk::delete_vtk_object_ $imageimport]

      #  The next section would deal with BAD pixels, if VisibilityConstraint
      #  worked with point data, current it doesn't and only works with cells.
      #  Left in place in case that changes. Note when that happens we could
      #  extend VisibilityConstriant to self-test the data, instead of
      #  using a visibility array.
      #
      #  Wrap with vtkUniformGrid with bespoke VisibilityConstraint to support
      #  blanking for some filters (not volume rendering). Need to just compare
      #  values with known BAD values, rather than use a blank array (unsigned
      #  char), but will require modifications to VTK so we can subclass.
      #  if { $visibility_check && ( ! $nobad ) } {
      #     set mask [::vtkUnsignedCharArray New]
      #     if { [gvtk::createmask $arrayinfo $mask] } {
      #        #  BAD pixels found. Do the wrap and set the mask array.
      #        set uniformgrid [::vtkUniformGrid New]
      #        #  Get data before we can copy.
      #        $imagedata Update
      #        $uniformgrid ShallowCopy $imagedata
      #        $uniformgrid SetPointVisibilityArray $mask
      #        $imagedata Delete
      #        $mask Delete
      #        set imagedata $uniformgrid
      #      }
      #   }
      return [list $imagedata $imageimport]
   }

   #  Simple proc to delete a VTK object. Use in callbacks.
   protected proc delete_vtk_object_ {obj} {
      $obj Delete
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}

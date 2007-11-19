#+
#  Name:
#     Gaia3dVtkCubeData

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Wrapper class for accessing vtkImageData instances.

#  Description:
#     Accesses the cube data in a instance of GaiaNDArray and makes
#     it available as a vtkImageData instance. Also provides facilities
#     for creating a sub-volume of the cube and handles the changes
#     to the world coordinate and apparent size.

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
#     16-NOV-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkCubeData {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { $imagedata_ != {} } {
         $imagedata_ Delete
      }
      if { $imageimport_ != {} } {
         $imageimport_ Delete
      }
   }

   #  Methods and procedures:
   #  -----------------------

   #  Re-access the data. Nothing is usually done until this is called so that
   #  unnecessary accesses are not performed.
   public method access {} {
      if { $cubeaccessor == {} } {
         return
      }

      #  Get the dimensionality.
      set dims [$cubeaccessor getdims 0]
      set ndims [llength $dims]

      #  Make sure the cube data is available.
      set arrayinfo [$cubeaccessor map "READ" "DATA"]

      #  Make vtkImageImport instance for assigning the imported data to a
      #  vtkImageData instance. 
      if { $imageimport_ != {} } {
         $imageimport_ Delete
      }
      set imageimport_ [::vtkImageImport New]
      set d1 [lindex $dims 0]
      set d2 [lindex $dims 1]
      set d3 [lindex $dims 2]
      incr d1 -1
      incr d2 -1
      incr d3 -1

      #  Dimensions.
      $imageimport_ SetWholeExtent 0 $d1 0 $d2 0 $d3
      $imageimport_ SetDataExtentToWholeExtent

      #  Do the sharing of the data from GAIA to VTK, if possible. 
      #  This normalises FITS data and returns if any bad values were 
      #  detected (strong truth), it can also replace any data values 
      #  with a null value (need to do this for volumes). If this happens
      #  then a copy of the data is made.
      set nobad [gvtk::setarray $arrayinfo $imageimport_ $checkbad $nullvalue]

      #  Read data into vktImageData.
      if { $imagedata_ != {} } {
         $imagedata_ Delete
      }
      set imagedata_ [$imageimport_ GetOutput]

      #  Origins are 1, not 0 for GRID coordinates. We also have a default
      #  spacing of 1,1,1.
      $imagedata_ SetOrigin 1.0 1.0 1.0

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
      #        $imagedata_ Update
      #        $uniformgrid ShallowCopy $imagedata_
      #        $uniformgrid SetPointVisibilityArray $mask
      #        $imagedata_ Delete
      #        $mask Delete
      #        set imagedata_ $uniformgrid
      #      }
      #   }
      return
   }
   
   #  Access the vtkImageData instance.
   public method get_imagedata {} {
      return $imagedata_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The GaiaNDArray instance that wraps the data-cube.
   public variable cubeaccessor {}

   #  Whether the data should be checked for bad values.
   public variable checkbad 0

   #  The value to use as a replacement for bad values.
   public variable nullvalue 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  VTK objects.
   protected variable imagedata_ {}
   protected variable imageimport_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

#  XXX code used to apply a VOI to the vtkImageData. Could use this instead
#  of extracting a sub-volume using GAIA. Kept for now.
#if { $limits_ != {} } {
#   set extractor [vtkExtractVOI New]
#   $extractor SetInput $imagedata_
#
#  Get limits along spectral axis.
#   set l1 [$itk_option(-gaiacube) axis_pixel2grid [lindex $limits 0]]
#   set l2 [$itk_option(-gaiacube) axis_pixel2grid [lindex $limits 1]]
#   set ll [expr min($l1,$l2)]
#   set ul [expr max($l1,$l2)]
#
#  And pick out other limits.
#   set axis [$itk_option(-gaiacube) get_axis]
#   lassign $dims_ nx ny yz
#   if { $axis == 1 } {
#      $extractor SetVOI $ll $ul 0 $ny 0 $nz
#      set dims_ "[expr $ul - $ll + 1] $ny $nz"
#   } elseif { $axis == 2 } {
#      $extractor SetVOI 0 $nx $ll $ul 0 $nz
#      set dims_ "$nx [expr $ul - $ll + 1] $nz"
#   } else {
#      $extractor SetVOI 0 $nx 0 $ny $ll $ul
#      set dims_ "$nx $ny [expr $ul - $ll + 1]"
#   }
#
#   $imagedata_ Delete
#   set imagedata_ [$extractor GetOutput]
#   #$extractor Delete;# XXX not yet.
#}

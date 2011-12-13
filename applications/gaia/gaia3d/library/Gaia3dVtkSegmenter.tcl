#+
#  Name:
#     Gaia3dVtkSegmenter

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Segment and display a 3d mask.

#  Description:
#     A class of objects that extract and display regions of the same value
#     from a mask. Masks usually contain contiguous regions associated with
#     some property which are identified by having the same integer value.
#
#     Each object is associated with a vtkImageData instance (the segmented
#     mask) and a render window. The properties associated with each object
#     are a set of mask values to identify and display, a lookup table to
#     colour the regions and an optional AST FrameSet. If given the FrameSet
#     represents a mapping from the coordinates of the vtkImageData instance
#     to that of the actual graphics coordinates (usually the GRID coordinates
#     of another cube that the mask represents).

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
#     20-MAY-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkSegmenter {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the necessary VTK objects.
      set segmenter_ [::vtkDiscreteMarchingCubes New]

      #  Create a mapper for the polygonal data.
      set mapper_ [::vtkPolyDataMapper New]
      $mapper_ SetInputConnection [$segmenter_ GetOutputPort]

      #  Observe the progress so that we can update the UI during long
      #  rendering jobs.
      $segmenter_ AddObserver ProgressEvent [code $this update_ui_]
      $mapper_ AddObserver ProgressEvent [code $this update_ui_]

      #  Create an actor for the polygonal data.
      set prop_ [::vtkLODActor New]
      $prop_ SetMapper $mapper_

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      $segmenter_ RemoveObserver ProgressEvent
      $mapper_ RemoveObserver ProgressEvent

      remove_from_window
      $prop_ Delete
      $mapper_ Delete
      $segmenter_ Delete
   }

   #  Methods and procedures:
   #  -----------------------

   #  Add this to the render window.
   public method add_to_window {} {
      $renwindow add_view_prop $prop_
   }

   #  Remove this from a render window.
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

   #  Set the values to extract and display. Each element of args is a
   #  value or a range of values (two values in a list element). The exception
   #  is if a single -1 is given. That means use all values in the mask.
   public method set_values {values} {

      if { $values != $last_values_ } {
         set values_changed_ 1
         set last_values_ $values
      } else {
         set values_changed_ 0
      }

      #  Clear all existing values.
      set nvalues_ 0
      $segmenter_ SetNumberOfContours 0

      #  All values.
      if {[llength $values] == 1 && $values == "-1" } {
         get_dataminmax_
         $segmenter_ GenerateValues [expr $dmax_-$dmin_+1] $dmin_ $dmax_
      } else {
         #  Given values and ranges.
         foreach value $values {
            if { [llength $value] == 2 } {
               set lb [lindex $value 0]
               set ub [lindex $value 1]
               for { set i $lb } { $i < $ub } { incr i } {
                  $segmenter_ SetValue $nvalues_ $i
                  incr nvalues_
               }
            } else {
               $segmenter_ SetValue $nvalues_ $value
               incr nvalues_
            }
         }
      }
   }

   #  Set the lookup table and opacity. Range of three lookup tables are
   #  avaiable random=1, rainbow=2 or grey=3.
   public method set_lut {n opacity} {
      set nlut_ [expr min(3,max(1,$n))]
      set opacity_ $opacity
      make_lut_
   }

   #  Set the opacity.
   public method set_opacity {opacity} {
      set opacity_ $opacity
      make_lut_
   }

   #  Make a lookup table of the current type to match the data range.
   protected method make_lut_ {} {
      get_dataminmax_

      set lut [vtkLookupTable New]
      $lut SetNumberOfColors [expr $dmax_-$dmin_+1]
      $lut SetTableRange $dmin_ $dmax_
      $lut SetScaleToLinear
      $lut Build

      if { $nlut_ == 3 } {
         #  Random grey.
         set math [vtkMath New]
         $math RandomSeed 5071
         for { set i $dmin_ } { $i <= $dmax_ } { incr i } {
            set value [$math Random .2 1]
            $lut  SetTableValue $i $value $value $value $opacity_
         }
         $math Delete
      } elseif { $nlut_ == 2 } {
         #  Rainbow, the default lut, just make sure we get all colours.
         $lut SetHueRange 0.0 1.0
         $lut SetAlphaRange $opacity_ $opacity_
      } else {
         #  Random colour.
         set math [vtkMath New]
         $math RandomSeed 5071
         for { set i $dmin_ } { $i <= $dmax_ } { incr i } {
            $lut  SetTableValue $i \
               [$math Random .2 1]  [$math Random .2 1]  [$math Random .2 1]  $opacity_
         }
         $math Delete
      }

      $mapper_ SetLookupTable $lut
      $mapper_ SetScalarRange $dmin_ $dmax_
   }

   #  Update the UI.
   protected method update_ui_ {} {
      ::update
   }

   #  Set the imagedata object. If blank remove visibility.
   protected method update_imagedata_ {} {
      $segmenter_ SetInput $imagedata
      if { $imagedata == {} } {
         remove_from_window
         set_invisible
      }
      set read_imagedata_ 0
   }

   #  Get the data min and max values. These are used to set the lookup tables
   #  and scalar ranges.
   protected method get_dataminmax_ {} {
      if { ! $read_imagedata_ } {
         #  Make sure imagedata is available.
         $imagedata Update
         lassign [$imagedata GetScalarRange] smin smax

         #  Lower value is usually BAD, so assume 0, or need to visit the raw cube.
         set dmin_ 0
         set dmax_ [expr int($smax)]

         #  Done for now.
         set read_imagedata_ 1
      }
   }

   #  WCS has changed. Need to re-connect the pipeline and update the
   #  transformation mapper.
   protected method wcs_changed_ {} {

      #  If last and this WCS are empty, or the same, then nothing to do.
      if { ($last_wcs_ == {} && $wcs == {}) || $last_wcs_ == $wcs } {
         return
      }

      if { $wcs == {} } {
         #  Removed WCS. Remove transform filter from pipeline.
         $mapper_ SetInputConnection [$segmenter_ GetOutputPort]
      }

      #  New WCS. XXX reuse vtkAstTransform for efficiency.
      set tpdf [::vtkTransformPolyDataFilter New]
      gvtk::astsetpolydatafiltertransform $tpdf $wcs

      #  Connection is contour to transform filter to mapper.
      $tpdf SetInputConnection [$segmenter_ GetOutputPort]
      $mapper_ SetInputConnection [$tpdf GetOutputPort]

      #  Don't need this command again, so delete.
      $tpdf Delete
   }

   #  Connect this to a given stencil filter (vtkPolyDataToImageStencil).
   #  Used to apply the current mask to some data (the surfaces in the
   #  discrete marching cubes are picked out by this and can be applied to
   #  another dataset attached to an vtkImageStencil).
   public method connect_stencil_filter {stencil_filter} {
      $stencil_filter SetInputConnection [$segmenter_ GetOutputPort]
   }

   #  See if the values changed last time values was called. Use as an
   #  indicator that the output from the stencil filter will change.
   public method changed {} {
      return $values_changed_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The render window (a Gaia3dVtkWindow instance). Not expected to change.
   public variable renwindow {}

   #  The current vtkImageData instance. Can be changed, must be set when
   #  first created.
   public variable imagedata {} {
      update_imagedata_
   }

   #  The optional WCS frameset. Re-connects the pipeline to use a mapper that
   #  transforms the polygon data.
   public variable wcs {} {
      wcs_changed_
      set last_wcs_ $wcs
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  VTK objects.
   protected variable prop_ {}
   protected variable mapper_ {}
   protected variable segmenter_ {}

   #  Value of last WCS variable.
   protected variable last_wcs_ {}

   #  Last values. Used to check if any changes are seen.
   protected variable last_values_ {}
   protected variable values_changed_ 0

   #  Current lut.
   protected variable nlut_ 1

   #  Whether imagedata has been read yet.
   protected variable read_imagedata_ 0

   #  Min and max integers in data.
   protected variable dmin_ 0
   protected variable dmax_ 0

   #  Opacity.
   protected variable opacity_ 1.0

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

#+
#  Name:
#     Gaia3dVtkIso

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate objects to display an isosurface.

#  Description:
#     A class of objects that represent an isosurface. Each isosurface
#     is associated with a vtkImageData instance (a data-cube) and
#     a render window. The properties associated with each surface are
#     its level, colour and opacity, plus an optional AST FrameSet.
#     If given the FrameSet represents a mapping from the coordinates of the
#     vtkImageData instance to that of the actual graphics coordinates
#     (usually the GRID coordinates of another cube).

#  Copyright:
#     Copyright (C) 2007-2009 Science and Technology Facilities Council
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
#     17-OCT-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkIso {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the necessary VTK objects.
      set contour_ [::vtkMarchingCubes New]

      #  Create a mapper for the polygonal data created to represent the iso
      #  surface.
      set mapper_ [::vtkPolyDataMapper New]
      $mapper_ SetInputConnection [$contour_ GetOutputPort]
      $mapper_ ScalarVisibilityOff

      #  Observe the progress of the mapper and contour so that we can update
      #  the UI during long rendering jobs.
      $contour_ AddObserver ProgressEvent [code $this update_ui_]
      $mapper_ AddObserver ProgressEvent [code $this update_ui_]

      #  Create an actor for the polygonal data.
      set prop_ [::vtkLODActor New]
      $prop_ SetMapper $mapper_
      set property_ [$prop_ GetProperty]

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      remove_from_window
      $prop_ Delete
      $mapper_ Delete
      $contour_ Delete
   }

   #  Methods and procedures:
   #  -----------------------

   #  Add this isosurface to the render window.
   public method add_to_window {} {
      $renwindow add_view_prop $prop_
   }

   #  Remove this iso surface from a render window.
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

   #  Set the level, colour and opacity in one call. More efficient.
   public method set_lco {inlevel incolour inopacity} {
      if { $level != $inlevel || $incolour != $colour ||
           $inopacity != $opacity } {

         #  OK, something changed. Record values and apply.
         set level $inlevel
         set colour $incolour
         set opacity $inopacity
         update_
      }
   }

   #  Update to the new level, colour and opacity.
   protected method update_ {} {
      $contour_ SetValue 0 $level
      set rgb [::gaia3d::Gaia3dVtkWindow::get_rgb_colour $colour]
      eval $property_ SetColor $rgb
      $property_ SetOpacity $opacity
   }

   #  Update the UI.
   protected method update_ui_ {} {
      ::update
   }

   #  Set the imagedata object. If blank remove visibility.
   protected method update_imagedata_ {} {

      #  If masking with a stencil connect using that to partition the data.
      if { $stencil == {} } {
         $contour_ SetInput $imagedata
      } else {
         $contour_ SetInputConnection [$stencil GetOutputPort]
      }

      if { $imagedata == {} } {
         remove_from_window
         set_invisible
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
         $mapper_ SetInputConnection [$contour_ GetOutputPort]

         #  Compute normals, better visual.
         $contour_ ComputeNormalsOn
         $contour_ ComputeGradientsOn
      }

      #  New WCS. XXX reuse vtkAstTransform for efficiency.
      set tpdf [::vtkTransformPolyDataFilter New]
      gvtk::astsetpolydatafiltertransform $tpdf $wcs

      #  Connection is contour to transform filter to mapper.
      $tpdf SetInputConnection [$contour_ GetOutputPort]
      $mapper_ SetInputConnection [$tpdf GetOutputPort]

      #  Don't compute normals, these do not transform.
      $contour_ ComputeNormalsOff
      $contour_ ComputeGradientsOff

      #  Don't need this command again, so delete.
      $tpdf Delete
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

   #  The vtkImageStencil if masking of the contours should be applied.
   public variable stencil {} {}

   #  The level.
   public variable level 0 {
      update_
   }

   #  The colour.
   public variable colour 0 {
      update_
   }

   #  The opacity.
   public variable opacity 1.0 {
      update_
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
   protected variable property_ {}
   protected variable mapper_ {}
   protected variable contour_ {}
   protected variable tpdf_ {}

   #  Value of last WCS variable.
   protected variable last_wcs_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

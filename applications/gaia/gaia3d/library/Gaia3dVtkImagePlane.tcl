#+
#  Name:
#     Gaia3dVtkImagePlane

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a vtkImagePlaneWidget

#  Description:

#     Class to create instances of a vtkImagePlaneWideget and provide methods
#     to manage the related work required to render it in a scene. This widget
#     is used to display a slice from a dataset (vtkImageData cube) and
#     provides default interactions to move and re-orient the slice within the
#     dataset. The plane always snaps to the nearest axis when
#     re-orienting. Commands can be evaluated when movement and axis
#     orientation changes occur.

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
#     08-AUG-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkImagePlane {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the widget
      set plane_ [::vtkImagePlaneWidget New]
      $plane_ DisplayTextOff

      #  Want to draw an image, so switch on the texture.
      $plane_ TextureVisibilityOn

      #  Outline colour.
      set_outline_colour 1 0 0

      #  Set up various events so we track movement and snapping. These
      #  can be propagated and used locally.
      $plane_ AddObserver StartInteractionEvent [code $this start_interact_]
      $plane_ AddObserver InteractionEvent [code $this interact_]
      $plane_ AddObserver EndInteractionEvent [code $this end_interact_]

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      remove_from_window
      $plane_ Delete
   }

   #  Methods and procedures:
   #  -----------------------

   #  Add this widget to the Gaia3dVtkWindow instance. Need to call this
   #  after object has been realized.
   public method add_to_window {} {
      $plane_ SetInteractor [$renwindow get_interactor]
      update
   }

   #  Remove this widget from a render window.
   public method remove_from_window {} {
      if { [$plane_ GetInteractor] != "" } {
         $plane_ Off
      }
   }

   #  Make visible. Adds all actors to scene.
   public method set_visible {} {
      if { [$plane_ GetInteractor] != "" } {
         $plane_ On
      }
   }

   #  Make invisible. Different to usual as removes component actors from
   #  scene.
   public method set_invisible {} {
      if { [$plane_ GetInteractor] != "" } {
         $plane_ Off
      }
   }

   #  Handle the setting of a dataset.
   protected method set_dataset_ {} {
      $plane_ SetInput $dataset
      $plane_ RestrictPlaneToVolumeOn
      update
   }

   #  Set the opacity.
   public method set_opacity {value} {
      [$plane_ GetTexturePlaneProperty] SetOpacity $value
   }

   #  Set the slice index to position the plane.
   public method set_slice_index {index} {
      $plane_ SetSliceIndex [expr int($index)]
   }

   #  Get the slice index of the plane.
   public method get_slice_index {} {
      return [$plane_ GetSliceIndex]
   }

   #  Determine if the current position is axis aligned. Do this
   #  by picking maximum normal direction which will be 1 for an aligned
   #  axis (note there is a chance of this being true while interacting,
   #  so we also check that the other axis have normals of 0).
   protected method is_axis_aligned_ {} {
      lassign [$plane_ GetNormal] x y z
      if { ( $x == 1 && $y == 0 && $z == 0 ) ||
           ( $x == 0 && $y == 1 && $z == 0 ) ||
           ( $x == 0 && $y == 0 && $z == 1 ) } {
         return 1
      }
      return 0
   }

   #  Set the colour of the outline.
   public method set_outline_colour {R G B} {
      [$plane_ GetPlaneProperty] SetColor $R $G $B
   }

   #  Set the axis.
   protected method set_axis_ {} {
      if { $axis == 1 } {
         $plane_ SetPlaneOrientationToXAxes
      } elseif { $axis == 2 } {
         $plane_ SetPlaneOrientationToYAxes
      } else {
         $plane_ SetPlaneOrientationToZAxes
      }
      update
   }

   #  Return if current position is valid, that is has coordinates
   #  and a data value.
   public method has_position {} {
      return [$plane_ GetCursorDataStatus]
   }

   #  Return the current coordinates.
   public method get_position {} {
      lassign [$plane_ GetCurrentCursorPosition] lastx_ lasty_ lastz_
      return "$lastx_ $lasty_ $lastz_"
   }

   #  Return the current position and change in position since last call to
   #  this routine or get_position.
   public method get_position_and_delta {} {
      lassign [$plane_ GetCurrentCursorPosition] x y z
      if { $lastx_ == -1.0 } {
         set dx 0.0
         set dy 0.0
         set dz 0.0
      } else {
         set dx [expr $x-$lastx_]
         set dy [expr $y-$lasty_]
         set dz [expr $z-$lastz_]
      }
      set x [expr int($x)]
      set y [expr int($y)]
      set z [expr int($z)]

      set lastx_ $x
      set lasty_ $y
      set lastz_ $z
      return "$x $y $z $dx $dy $dz"
   }

   #  Get the current data value.
   public method get_value {} {
      return [$plane_ GetCurrentImageValue]
   }

   #  Handle the start of interaction event. May initiate a callback.
   protected method start_interact_ {} {
      #  Zero deltas.
      set lastx_ -1.0
      if { $start_interact_cmd != {} } {
         eval $start_interact_cmd
      }
   }

   #  Handle an interaction on the plane. May initiate two callbacks,
   #  interact_cmd always happens, if set, and move_cmd if a valid position
   #  is not available (not tracking) and we're axis aligned, so we're moving
   #  along axis.
   protected method interact_ {} {
      if { $interact_cmd != {} } {
         eval $interact_cmd
      }
      if { $move_cmd != {} } {
         if { ! [has_position] && [is_axis_aligned_] } {
            eval $move_cmd [get_slice_index]
         }
      }
   }

   #  Handle an interaction end event. May initiate two callbacks,
   #  end_interact_cmd always happens, if set, and snap_cmd if a valid
   #  position is available and the plane has been rotated (and thus snapped
   #  back to an axis).
   protected method end_interact_ {} {
      if { $end_interact_cmd != {} } {
         eval $end_interact_cmd
      }

      #  Do the snap, if needed. First pick maximum normal direction.
      lassign [$plane_ GetNormal] x y z
      set x [expr abs($x)]
      set y [expr abs($y)]
      set z [expr abs($z)]
      set m [::max $x $y $z]

      #  If this is exactly 1 then we're already aligned, so do nothing (could
      #  happen, but unlikely).
      if { $m != 1 } {

         #  Get current position of centre, want to preserve this as the new
         #  slice position.
         lassign [$plane_ GetOrigin] xo yo zo

         #  Snap to axis and set the slice position.
         if { $m == $x } {
            set axis 1
            set index [expr int($xo)]
            $plane_ SetPlaneOrientationToXAxes
         } elseif { $m == $y } {
            set axis 2
            set index [expr int($yo)]
            $plane_ SetPlaneOrientationToYAxes
         } else {
            set axis 3
            set index [expr int($zo)]
            $plane_ SetPlaneOrientationToZAxes
         }
         $plane_ SetSliceIndex $index

         #  Issue snap callback command.
         if { $snap_cmd != {} } {
            eval $snap_cmd $axis $index
         }
      }
   }

   #  Update the image widget to use the current data limits and colour
   #  table.
   public method update {} {
      if { $rtdimage != {} } {

         if { $lookup_ == {} } {
            set lookup_ [vtkLookupTable New]
            $plane_ SetLookupTable $lookup_
         }

         lassign [$rtdimage cut] low high
         $lookup_ SetTableRange $low $high

         #  Choose colour map for lookup table.
         switch -exact $colourmap {
            "white" {
               set cmap "allwhite.lasc"
            }
            "black" {
               set cmap "allblack.lasc"
            }
            "grey" {
               set cmap "ramp.lasc"
            }
            "image" {
               set cmap [$rtdimage cmap file]
            }
            default {
               set cmap $colourmap
            }
         }
         gvtk::cmap set $cmap $lookup_

         #  Need to get plane to redraw as we've not modified it directly.
         $plane_ UpdatePlacement
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The render window (a Gaia3dVtkWindow instance). Required.
   public variable renwindow {}

   #  The current vtkImageData instance.
   public variable dataset {} {
      set_dataset_
   }

   #  An rtdimage instance. Used to set the black and white limits and
   #  colourmap.
   public variable rtdimage {} {
      update
   }

   #  The opacity.
   public variable opacity {} {
      set_opacity $opacity
   }

   #  Type of colour map to use in the lookup table. One of "black",
   #  "white", "grey", "image" or the name of an installed colourmap.
   public variable colourmap "grey"

   #  Whether to align to an axis.
   public variable align_to_axis 0

   #  The axis to align to.
   public variable axis 3 {
      set_axis_
   }

   #  Command to execute on the start of an interaction.
   public variable start_interact_cmd {}

   #  Command to execute during an interaction.
   public variable interact_cmd {}

   #  Command to execute at the end of an interaction.
   public variable end_interact_cmd {}

   #  Command to execute when the plane is moved. Will be qualified with the
   #  slice index.
   public variable move_cmd {}

   #  Command to execute when the plane is snapped to an axis. Will be
   #  qualified by the new axis and the slice index.
   public variable snap_cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  VTK objects.
   protected variable plane_ {}
   protected variable lookup_ {}

   #  Previously reported position.
   protected variable lastx_ -1.0
   protected variable lasty_ -1.0
   protected variable lastz_ -1.0

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

#+
#  Name:
#     Gaia3dVtkAxes

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create an simple representation of the axes of a cube.

#  Description:
#     Draws three simple axes, with arrow heads, pointing in the
#     directions of the cube axes and labels them with the values from
#     the associated WCS.

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
#     06-SEP-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkAxes {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the axes.
      set axes_ [::vtkAxesActor New]

      #  Some basic properties.
      $axes_ SetTotalLength 1.0 1.0 1.0
      $axes_ SetConeRadius 0.2
      $axes_ SetNormalizedTipLength 0.1 0.1 0.1

      #  Text properties
      set tprop [vtkTextProperty New]
      $tprop ItalicOn
      $tprop SetFontFamilyToTimes
      $tprop SetColor 0.0 0.0 1.0
      $tprop SetFontSize 15

      set xca [$axes_ GetXAxisCaptionActor2D]
      $xca SetCaptionTextProperty $tprop

      set yca [$axes_ GetYAxisCaptionActor2D]
      $yca SetCaptionTextProperty $tprop

      set zca [$axes_ GetZAxisCaptionActor2D]
      $zca SetCaptionTextProperty $tprop

      $tprop Delete

      #  Don't want the text scaled to fit each axes independently.
      [$xca GetTextActor] SetTextScaleModeToNone
      [$yca GetTextActor] SetTextScaleModeToNone
      [$zca GetTextActor] SetTextScaleModeToNone

      #  Activate the orientation marker.
      set marker_ [vtkOrientationMarkerWidget New]
      $marker_ SetOutlineColor 0.93 0.57 0.13
      $marker_ SetOrientationMarker $axes_
      $marker_ SetViewport 0.0 0.0 1.0 1.0

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      remove_from_window
      $axes_ Delete
      $marker_ Delete
   }

   #  Methods and procedures:
   #  -----------------------

   #  Add this line to the render window.
   public method add_to_window {} {
      #  Do nothing, wait for set_visible.
   }

   #  Remove this line from a render window?
   public method remove_from_window {} {
      $marker_ SetEnabled 0
   }

   #  Make line visible.
   public method set_visible {} {
      $marker_ SetInteractor [$renwindow get_interactor]
      $marker_ SetEnabled 1
      $marker_ InteractiveOff
   }

   #  Make line invisible.
   public method set_invisible {} {
      $marker_ SetEnabled 0
   }

   #  Orientate the axes to point along the WCS directions, not the cube
   #  axes.
   protected method orient_axes_ {} {

      #  Project two positions in grid coordinates. These points
      #  need to be valid.
      lassign [gaiautils::asttrann $wcs 1 "0 0 0"] x0 y0 z0
      lassign [gaiautils::asttrann $wcs 1 "1 1 1"] x1 y1 z1

      #  Use shifts in each axes to determine the WCS directions by getting
      #  the end points (in GRID coords from centre above).
      lassign [gaiautils::asttrann $wcs 0 "$x1 $y0 $z0"] p1x p1y p1z
      lassign [gaiautils::asttrann $wcs 0 "$x0 $y1 $z0"] p2x p2y p2z
      lassign [gaiautils::asttrann $wcs 0 "$x0 $y0 $z1"] p3x p3y p3z

      #  Normalise into unit vectors.
      lassign [normalise $p1x $p1y $p1z] p1x p1y p1z
      lassign [normalise $p2x $p2y $p2z] p2x p2y p2z
      lassign [normalise $p3x $p3y $p3z] p3x p3y p3z

      #  Convert these into orientations.
      catch {
         set matrix [vtkMatrix4x4 New]

         if { $x0 > $x1 } {
            $matrix SetElement 0 0 [expr $p1x*-1]
            $matrix SetElement 1 0 $p2x
            $matrix SetElement 2 0 $p3x
         } else {
            $matrix SetElement 0 0 $p1x
            $matrix SetElement 1 0 [expr $p2x*-1]
            $matrix SetElement 2 0 [expr $p3x*-1]
         }

         if { $y0 > $y1 } {
            $matrix SetElement 0 1 $p1y
            $matrix SetElement 1 1 [expr $p2y*-1]
            $matrix SetElement 2 1 [expr $p3y*-1]
         } else {
            $matrix SetElement 0 1 [expr $p1y*-1]
            $matrix SetElement 1 1 $p2y
            $matrix SetElement 2 1 $p3y
         }

         if { $z0 > $z1 } {
            $matrix SetElement 0 2 [expr $p1z*-1]
            $matrix SetElement 1 2 [expr $p2z*-1]
            $matrix SetElement 2 2 [expr $p3z*-1]
         } else {
            $matrix SetElement 0 2 $p1z
            $matrix SetElement 1 2 $p2z
            $matrix SetElement 2 2 $p3z
         }

         $axes_ SetUserMatrix $matrix
         $matrix Delete

      }
   }

   #  Set the axis labels. Done when a WCS is defined.
   protected method set_axes_labels_ {} {
      if { [catch {
         $axes_ SetXAxisLabelText [gaiautils::astget $wcs "label(1)"]
      }] } {
         $axes_ SetXAxisLabelText "X"
      }
      if { [catch {
         $axes_ SetYAxisLabelText [gaiautils::astget $wcs "label(2)"]
      }] } {
         $axes_ SetYAxisLabelText "Y"
      }
      if { [catch {
         $axes_ SetZAxisLabelText [gaiautils::astget $wcs "label(3)"]
      }] } {
         $axes_ SetZAxisLabelText "Z"
      }
   }

   #  Normalize a vector into a unit vector.
   public proc normalise {x y z} {
      set d [modulus $x $y $z]
      if { $d > 0.0 } {
         return [list [expr $x/$d] [expr $y/$d] [expr $z/$d]]
      }
      return [list $x $y $z]
   }

   #  Return the length of a vector.
   public proc modulus {x y z} {
      return [expr sqrt($x*$x + $y*$y + $z*$z)]
   }



   #  Configuration options: (public variables)
   #  ----------------------

   #  The render window (a Gaia3dVtkWindow instance).
   public variable renwindow {}

   #  The AST FrameSet describing the axes.
   public variable wcs {} {
      orient_axes_
      set_axes_labels_
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  VTK objects.
   protected variable axes_
   protected variable marker_

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

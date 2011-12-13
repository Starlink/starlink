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

      #  Get a linear transformation to represent the decomposition
      #  into scales and direction vectors.
      lassign [gaiautils::astlinearapprox $wcs {0 0 0} {1 1 1} 1] \
         X0 Y0 Z0 X1 Y1 Z1 X2 Y2 Z2 X3 Y3 Z3

      #  Look for a longtude axis. If that is formatted in time we
      #  need to correct all its scale factors for a term cos(dec)
      #  where dec is the associated latitude axis.
      catch {
         set lonaxis [gaiautils::astget $wcs "lonaxis"]
         set lataxis [gaiautils::astget $wcs "lataxis"]

         #  If the lonaxis is in time.
         set astime [gaiautils::astget $wcs "astime($lonaxis)"]
         if { $astime } {

            #  Project a position into world coordinates. Needed to
            #  establish a cos(dec).
            lassign [gaiautils::asttrann $wcs 1 "0 0 0"] dec(1) dec(2) dec(3)
            set cosdec [expr cos($dec($lataxis))]

            #  Scale to offset degrees.
            if { $lonaxis == 1 } {
               set X1 [expr $X1*$cosdec]
               set Y1 [expr $Y1*$cosdec]
               set Z1 [expr $Z1*$cosdec]
            } elseif { $lonaxis == 2 } {
               set X2 [expr $X2*$cosdec]
               set Y2 [expr $Y2*$cosdec]
               set Z2 [expr $Z2*$cosdec]
            } elseif { $lonaxis == 3 } {
               set X3 [expr $X3*$cosdec]
               set Y3 [expr $Y3*$cosdec]
               set Z3 [expr $Z3*$cosdec]
            }
         }
      }

      #  Normalise, removes scale factors so we just have directions.
      lassign [normalise $X1 $X2 $X3] X1 X2 X3
      lassign [normalise $Y1 $Y2 $Y3] Y1 Y2 Y3
      lassign [normalise $Z1 $Z2 $Z3] Z1 Z2 Z3

      #  Set direction vectors as components of matrix and apply to axes.
      set matrix [vtkMatrix4x4 New]

      $matrix SetElement 0 0 $X1
      $matrix SetElement 1 0 $Y1
      $matrix SetElement 2 0 $Z1

      $matrix SetElement 0 1 $X2
      $matrix SetElement 1 1 $Y2
      $matrix SetElement 2 1 $Z2

      $matrix SetElement 0 2 $X3
      $matrix SetElement 1 2 $Y3
      $matrix SetElement 2 2 $Z3

      $axes_ SetUserMatrix $matrix
      $matrix Delete
      return
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

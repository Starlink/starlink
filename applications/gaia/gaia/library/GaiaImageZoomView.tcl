#+
#  Name:
#     GaiaImageZoomView

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     GAIA extensions to the RtdImageZoomView class.

#  Description:
#     This class extends RtdImageZoomView to add the facilities required in
#     GAIA. Currently that is limited to extending the zoomed view so that when
#     a small image is displayed the zoom is adaptively increased so that the
#     zoom window is fullsized. This makes sure that the central pixel
#     of the zoomed window is displaying something, which is required.

#  Invocations:
#
#        GaiaImageZoomView object_name [configuration options]
#
#     This creates an instance of a GaiaImageZoomView object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#     See declarations below.

#  Methods:
#     See declarations below.

#  Inheritance:
#     RtdImageZoomView

#  Copyright:
#     Copyright (C) 2007 Science and Technology Facilities Council.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     04-JUN-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaImageZoomView {}

itcl::class gaia::GaiaImageZoomView {

   #  Inheritances:
   #  -------------
   inherit rtd::RtdImageZoomView

   #  Constructor:
   #  ------------
   constructor {args} {
      #  Evaluate any options [incr Tk].
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Called when the main image is scaled to draw a box around the center
   #  pixel. Override so real method uses adaptive scale factor.
   public method scale {} {
      set old_factor $itk_option(-factor)
      set itk_option(-factor) $factor_
      rtd::RtdImageZoomView::scale
      set itk_option(-factor) $old_factor
   }

   #  Called when the zoom checkbutton is pressed and when image is entered.
   #  Override to use the adaptive scale factor.
   public method zoom {{clear 0}} {
      set old_factor $itk_option(-factor)
      set itk_option(-factor) $factor_
      rtd::RtdImageZoomView::zoom $clear
      set itk_option(-factor) $old_factor
   }

   #  This method is called when the mouse ptr enters an RtdImage. Sets the
   #  target scale factor from the given rtdimage.
   #  Override so that an adjusted scale factor is determined when the zoomed
   #  image doesn't fill the zoomed window. This is needed as the central
   #  pixel of the zoomed window has to show the image.
   public method enter_image {image} {

      #  Calculate adjusted scale factor.
      set w [$image_ width]
      set h [$image_ height]
      if { $itk_option(-propagate) && $w > 0 && $h > 0 } {

         #  Scale used in main window (*itk_option(-factor) is expected scale).
         lassign [$image scale] target_scale_
         if {$target_scale_ < 1} {
            set target_scale_ 1
         }

         #  Scales needed to fill window.
         set fw [expr 1+$itk_option(-width)/($w*$target_scale_)]
         set fh [expr 1+$itk_option(-height)/($h*$target_scale_)]

         #  Pick largest, but never smaller than the expected scale factor.
         set f [max $fw $fh $itk_option(-factor)]

         #  To be safe, never need to scale to show more than one pixel.
         set f [min $f $itk_option(-width)]
         set factor_ [expr int($f)]
      }

      #  Do real work.
      rtd::RtdImageZoomView::enter_image $image
   }

   #  Reset adaptive zoom when leaving image, so it is not reused
   #  without being updated.
   public method leave_image {image} {
      set factor_ $itk_option(-factor)
      rtd::RtdImageZoomView::leave_image $image
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Adaptive scale factor in use.
   protected variable factor_ 4

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}

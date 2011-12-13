#+
#  Name:
#     Gaia3dVtkOverlayText

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Create and manipulate a text prop

#  Description:
#     Class to create instances of a vtkTextActor and provide methods to
#     manage the related work required to render it in a scene. The TextActor
#     is 2D and designed for displaying coordinates, titles etc. The
#     positioning is with a normalized coordinate system of the viewport and
#     overlays the rendered scene.

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
#     13-AUG-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtkOverlayText {

   #  Inheritances:
   #  -------------

   #  None.

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the prop.
      set text_ [::vtkTextActor New]

      #  Make it suitable for overlaying and positioning with the viewport.
      $text_ SetTextScaleModeToNone

      set prop [$text_ GetTextProperty]
      $prop SetJustificationToLeft
      $prop SetVerticalJustificationToBottom

      #  Simple text.
      $prop SetColor 1 0 0
      $prop SetFontFamilyToArial
      $prop SetFontSize 12
      $prop BoldOff
      $prop ItalicOff
      $prop ShadowOff

      #  Initial position.
      set_position 0.01 0.01

      #  Set any configuration variables.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      remove_from_window
      $text_ Delete
   }

   #  Methods and procedures:
   #  -----------------------

   #  Set the text/
   public method set_text {text} {
      $text_ SetInput "$text"
      $text_ Modified
   }

   #  Add to the render window.
   public method add_to_window {} {
      $renwindow add_view_prop $text_
   }

   #  Remove from the render window.
   public method remove_from_window {} {
      $renwindow remove_view_prop $text_
   }

   #  Make text visible.
   public method set_visible {} {
      $text_ VisibilityOn
   }

   #  Make text invisible.
   public method set_invisible {} {
      $text_ VisibilityOff
   }

   #  Set the position of the text. An X,Y position in normalized viewport
   #  coordinates (0 -> 1).
   public method set_position {x y} {
      set coord [$text_ GetPositionCoordinate]
      $coord SetCoordinateSystemToNormalizedViewport
      $coord SetValue $x $y
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The render window (a Gaia3dVtkWindow instance).
   public variable renwindow {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  VTK objects.
   protected variable text_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

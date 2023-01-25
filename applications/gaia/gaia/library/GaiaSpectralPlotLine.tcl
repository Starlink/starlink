#+
#  Name:
#     GaiaSpectralPlotLine

#  Type of Module:
#     [incr Tk] widget

#  Purpose:
#     A multi-featured control for displaying a spectral coordinate
#     associated with a GaiaCube instance.

#  Description:

#     This control consists of several elements, a LabelEntryScale for picking
#     a grid position along a spectral axis, plus labels for displaying the
#     effective world coordinate. If defined in the associated GaiaCube
#     instance this coordinate can be used to position a graphical element,
#     (usually displayed as a vertical line in an associated GaiaSpectralPlot).

#  Invocations:
#
#        GaiaSpectralPlotLine object_name [configuration options]
#
#     This creates an instance of a GaiaSpectralPlotLine object. The
#     return is the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See itk_option definitions below.

#  Methods:
#     See declarations.

#  Inheritance:
#     util::FrameWidget

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
#     17-MAY-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSpectralPlotLine {}

itcl::class gaia::GaiaSpectralPlotLine {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Slider that controls the position along a spectral axis.
      itk_component add index {
         util::LabelEntryScale $w_.index \
            -text "$itk_option(-text)" \
            -value $itk_option(-value) \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from $itk_option(-from) \
            -to $itk_option(-to) \
            -increment $itk_option(-increment) \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -fix_range 1 \
            -command [code $this picked_plane_]
      }
      pack $itk_component(index) -side top -fill x

      #  Use these bindings to offer an indication that the interaction has
      #  halted (or isn't going to start a long interaction). The caller can
      #  use this to do jobs that require long processing, but are not
      #  necessary when updating quickly.
      $itk_component(index) bindscale <ButtonRelease-1> [code $this drag_end_]
      $itk_component(index) bindentry <Return> [code $this drag_end_]

      #  Index coordinate.
      itk_component add indexcoord {
         util::LabelValue $w_.indexcoord \
            -text "$itk_option(-coordtext)" \
            -labelwidth $itk_option(-labelwidth)
      }
      pack $itk_component(indexcoord) -side top -fill x

      #  Index coordinate type.
      if { $itk_option(-show_type) } {
         itk_component add indextype {
            util::LabelValue $w_.indextype \
               -text "$itk_option(-coordtypetext)" \
               -labelwidth $itk_option(-labelwidth)
         }
         pack $itk_component(indextype) -side top -fill x
      }

      #  Increment of arrows.
      if { $itk_option(-show_increment) } {
         itk_component add increment {
            util::LabelEntryScale $w_.increment \
               -text {Increment:} \
               -value 1 \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -from 1 \
               -to 100 \
               -increment 5 \
               -resolution 1 \
               -show_arrows 1 \
               -anchor w \
               -delay 25 \
               -command [code $this configure -increment]
         }
         pack $itk_component(increment) -side top -fill x
         add_short_help $itk_component(increment) {Increment used by arrows}
      }
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  The slider position has changed. Update the displayed world coordinate
   #  and issue the coord changed command.
   protected method picked_plane_ { plane } {
      update_coords_ $plane

      #  Make change known.
      if { $itk_option(-coord_update_cmd) != {} } {
         eval $itk_option(-coord_update_cmd) $plane
      }
   }

   #  A drag has completed. Do actions that are expensive.
   protected method drag_end_ {} {
      if { $itk_option(-drag_update_cmd) != {} } {
         eval $itk_option(-drag_update_cmd)
      }
   }

   #  Update the coordinates readout to show values for a plane.
   protected method update_coords_ {plane} {
      set coord [{*}$itk_option(-gaiacube) get_coord $plane 1 0]
      $itk_component(indexcoord) configure -value $coord

      #  Reposition a related reference line in a spectral plot. Note this
      #  must be unformatted.
      if { $itk_option(-show_ref_line) } {
         set coord [{*}$itk_option(-gaiacube) get_coord $plane 0 0]
         {*}$itk_option(-gaiacube) set_spec_ref_coord $itk_option(-ref_id) $coord
      }
   }

   #  Update the coordinates readout to show the coordinate type.
   #  This is expensive so should be handled by the caller when needed
   #  (change of cube axis, new dataset etc.).
   public method update_coords_type {plane} {
      if { $itk_option(-show_type) } {
         set vlu [{*}$itk_option(-gaiacube) get_coord $plane 1 1]
         set trail [lassign $vlu value]
         $itk_component(indextype) configure -value $trail
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The GaiaCube instance we're being used from.
   itk_option define -gaiacube gaiacube GaiaCube {}

   #  The reference line identifier for this component. Passed back in call
   #  to set_spec_ref_coord.
   itk_option define -ref_id ref_id Ref_Id 1

   #  Text for the main label.
   itk_option define -text text Text "Index of plane:"

   #  Text for the coordinate label.
   itk_option define -coordtext coordtext CoordText "Coordinate of plane:"

   #  Text for the coordinate type label.
   itk_option define -coordtypetext coordtypetext CoordTypeText \
      "Coordinate type:"

   #  The current index.
   itk_option define -value value Value 1 {
      if { [info exists itk_component(index)] } {
         $itk_component(index) configure -value $itk_option(-value)
         update_coords_ $itk_option(-value)
      }
   }

   #  Upper and lower indices.
   itk_option define -from from From 1 {
      if { [info exists itk_component(index)] } {
         $itk_component(index) configure -from $itk_option(-from)
      }
   }

   itk_option define -to to To 100 {
      if { [info exists itk_component(index)] } {
         $itk_component(index) configure -to $itk_option(-to)
      }
   }

   #  Width of any labels.
   itk_option define -labelwidth labelwidth LabelWidth 20

   #  Width of any the value of LabelEntryScale
   itk_option define -valuewidth valuewidth ValueWidth 20

   #  Command to perform when the coordinate changes (update image...).
   itk_option define -coord_update_cmd coord_update_cmd Coord_Update_Cmd {}

   #  Command to perform when dragging of slider stops (update wcs...).
   itk_option define -drag_update_cmd drag_update_cmd Drag_Update_Cmd {}

   #  Whether to include the type description label.
   itk_option define -show_type show_type Show_Type 0

   #  Whether to show updates to the reference line.
   itk_option define -show_ref_line show_ref_line Show_Ref_Line 0

   #  Whether to show the control for setting the increment.
   itk_option define -show_increment show_increment Show_Increment 0

   #  The increment of the slider. Used to skip more quickly.
   itk_option define -increment increment Increment 1 {
      if { [info exists itk_component(index)] } {
         $itk_component(index) configure -increment $itk_option(-increment)
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}

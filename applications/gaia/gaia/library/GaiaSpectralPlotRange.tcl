#+
#  Name:
#     GaiaSpectralPlotRange

#  Type of Module:
#     [incr Tk] widget

#  Purpose:
#     A multi-featured control for displaying a spectral coordinate range
#     associated with a GaiaCube instance.

#  Description:
#     This control consists of several elements, two LabelEntryScales for
#     picking two grid positions along a spectral axis, plus labels for
#     displaying the effective world coordinates. If defined in the associated
#     GaiaCube instance the coordinates can be used to position a graphical
#     element, (usually displayed as an xrange GaiaSpectralPlot).

#  Invocations:
#
#        GaiaSpectralPlotRange object_name [configuration options]
#
#     This creates an instance of a GaiaSpectralPlotRange object. The
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
#     26-MAY-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSpectralPlotRange {}

itcl::class gaia::GaiaSpectralPlotRange {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  First slider that controls the lower bound.
      itk_component add index1 {
         util::LabelEntryScale $w_.index1 \
            -text "$itk_option(-text1)" \
            -value $itk_option(-value1) \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from $itk_option(-from) \
            -to $itk_option(-to) \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -fix_range 1 \
            -command [code $this picked_plane1_]
      }
      pack $itk_component(index1) -side top -fill x

      #  Coordinates of index.
      itk_component add indexcoord1 {
         util::LabelValue $w_.indexcoord1 \
            -text "$itk_option(-coordtext1)" \
            -labelwidth $itk_option(-labelwidth)
      }
      pack $itk_component(indexcoord1) -side top -fill x

      #  Second slider that controls the upper bound.
      itk_component add index2 {
         util::LabelEntryScale $w_.index2 \
            -text "$itk_option(-text2)" \
            -value $itk_option(-value2) \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from $itk_option(-from) \
            -to $itk_option(-to) \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -fix_range 1 \
            -command [code $this picked_plane2_]
      }
      pack $itk_component(index2) -side top -fill x

      #  Coordinates of index.
      itk_component add indexcoord2 {
         util::LabelValue $w_.indexcoord2 \
            -text "$itk_option(-coordtext2)" \
            -labelwidth $itk_option(-labelwidth)
      }
      pack $itk_component(indexcoord2) -side top -fill x

      #  Use these bindings to offer an indication that the interaction has
      #  halted (or isn't going to start a long interaction). The caller can
      #  use this to do jobs that require long processing, but are not
      #  necessary when updating quickly.
      $itk_component(index1) bindscale <ButtonRelease-1> [code $this drag_end_]
      $itk_component(index1) bindentry <Return> [code $this drag_end_]
      $itk_component(index2) bindscale <ButtonRelease-1> [code $this drag_end_]
      $itk_component(index2) bindentry <Return> [code $this drag_end_]
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  The slider1 position has changed.
   protected method picked_plane1_ { plane1 } {
      set plane2 [$itk_component(index2) cget -value]
      apply_changes_ $plane1 $plane2
   }

   #  The slider2 position has changed.
   protected method picked_plane2_ { plane2 } {
      set plane1 [$itk_component(index1) cget -value]
      apply_changes_ $plane1 $plane2
   }

   #  Update the displayed world coordinate and issue the coord changed
   #  command.
   protected method apply_changes_ {plane1 plane2} {
      update_coords_ $plane1 $plane2

      #  Make change known.
      if { $itk_option(-coord_update_cmd) != {} } {
         eval $itk_option(-coord_update_cmd) $plane1 $plane2
      }
   }

   #  A drag has completed. Do actions that are expensive.
   protected method drag_end_ {} {
      if { $itk_option(-drag_update_cmd) != {} } {
         eval $itk_option(-drag_update_cmd)
      }
   }

   #  Update the coordinates readout to show values for two planes.
   protected method update_coords_ {plane1 plane2} {
      set coord1 [{*}$itk_option(-gaiacube) get_coord $plane1 1 0]
      set coord2 [{*}$itk_option(-gaiacube) get_coord $plane2 1 0]
      $itk_component(indexcoord1) configure -value $coord1
      $itk_component(indexcoord2) configure -value $coord2

      #  Reposition a related reference range in a spectral plot. Note these
      #  coordinates must be unformatted.
      if { $itk_option(-show_ref_range) } {
         set coord1 [{*}$itk_option(-gaiacube) get_coord $plane1 0 0]
         set coord2 [{*}$itk_option(-gaiacube) get_coord $plane2 0 0]
         {*}$itk_option(-gaiacube) set_spec_ref_range_coord $itk_option(-ref_id) \
            $coord1 $coord2
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The GaiaCube instance we're being used from.
   itk_option define -gaiacube gaiacube GaiaCube {}

   #  The reference range identifier for this component. Passed back in call
   #  to set_spec_ref_range_coord.
   itk_option define -ref_id ref_id Ref_Id 1

   #  Text for the main labels.
   itk_option define -text1 text1 Text1 "Index of plane1:"
   itk_option define -text2 text2 Text2 "Index of plane2:"

   #  Text for the coordinate labels.
   itk_option define -coordtext1 coordtext1 CoordText1 "Coordinate of plane:"
   itk_option define -coordtext2 coordtext2 CoordText2 "Coordinate of plane:"

   #  The index pairs. These are clipped if necessary.
   itk_option define -value1 value1 Value1 1 {
      if { [info exists itk_component(index1)] } {
         $itk_component(index1) configure -value $itk_option(-value1)
         set v [$itk_component(index1) cget -value]
         if { $v != $itk_option(-value1) } {
            set itk_option(-value1) $v
         }
         update_coords_ $itk_option(-value1) $itk_option(-value2)
      }
   }
   itk_option define -value2 value2 Value2 1 {
      if { [info exists itk_component(index2)] } {
         $itk_component(index2) configure -value $itk_option(-value2)
         set v [$itk_component(index2) cget -value]
         if { $v != $itk_option(-value2) } {
            set itk_option(-value2) $v
         }
         update_coords_ $itk_option(-value1) $itk_option(-value2)
      }
   }

   #  Upper and lower indices, same for both controls.
   itk_option define -from from From 1 {
      if { [info exists itk_component(index1)] } {
         $itk_component(index1) configure -from $itk_option(-from)
         $itk_component(index2) configure -from $itk_option(-from)
      }
   }

   itk_option define -to to To 100 {
      if { [info exists itk_component(index1)] } {
         $itk_component(index1) configure -to $itk_option(-to)
         $itk_component(index2) configure -to $itk_option(-to)
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

   #  Whether to show updates to the reference range.
   itk_option define -show_ref_range show_ref_range Show_Ref_Range 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}

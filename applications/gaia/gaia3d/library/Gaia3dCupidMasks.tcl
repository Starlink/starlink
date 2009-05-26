#+
#  Name:
#     Gaia3dCupidMasks

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Add additional CUPID masks into a scene.

#  Description:
#     Series of controls for selecting CUPID cubes, choosing the values
#     to display and how to colour them.
#
#     The coordinate system used to match these cubes to the primary one
#     associated with the Gaia3dTool instance can be made to take into
#     account transformations between sky and spectral coordinates.

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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     20-MAY-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual Gaia3dCupidMasks {}

itcl::class ::gaia3d::Gaia3dCupidMasks {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Different UI layout for 1 cube.
      if { $itk_option(-maxcubes) == 1 } {
         itk_component add attrule {
            LabelRule $w_.attrule \
               -text "Mask & attributes:"
         }
         pack $itk_component(attrule) -side top -fill x -expand 1
      }

      #  Add coordinate matching options to the given menu.
      if { $itk_option(-options_menu) != {} } {

         #  Assume menu is associated with the same top-level as this object.
         set top [winfo toplevel $w_]

         $itk_option(-options_menu) add checkbutton \
            -label {Match coordinates} \
            -variable [scope match_] \
            -onvalue 1 \
            -offvalue 0
         $top add_menu_short_help $itk_option(-options_menu) \
            {Match coordinates} \
            {Determine transformations to the main cube coordinates}

         #  Matching with knowledge of the sidebands.
         $itk_option(-options_menu) add checkbutton \
            -label {Match sidebands} \
            -variable [scope match_sidebands_] \
            -onvalue 1 \
            -offvalue 0
         $top add_menu_short_help $itk_option(-options_menu) \
            {Match sidebands} \
            {Match spectral axes using sidebands}

         #  Matching using the base system, useful for velocities.
         $itk_option(-options_menu) add checkbutton \
            -label {Match using base system} \
            -variable [scope match_base_system_] \
            -onvalue 1 \
            -offvalue 0
         $top add_menu_short_help $itk_option(-options_menu) \
            {Match using base system} \
            {Match spectral axes using current coordinate system}
      }

      #  Main controls.
      add_controls_
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Release all accessed cubes.
      for { set i 0 } { $i < $itk_option(-maxcubes) } { incr i } {
         if { [info exists cubeaccessor_($i)] } {
            release_cube_ $i
            ::delete object $cubeaccessor_($i)
         }
      }
      catch {release_vtk_objects}
   }

   #  Methods and procedures:
   #  -----------------------

   #  Release all data associated with a cube.
   protected method release_cube_ {index} {
      if { [info exists cubeaccessor_($index)] } {
         $cubeaccessor_($index) close
         ::delete object $imagedata_($index)
         set imagedata_($index) {}
      }
   }

   #  Render any masks we have into a given renderer. The target coordinates
   #  are defined by the supplied AST FrameSet.
   public method render {renwindow target_wcs} {

      #  Make sure all cube data is available. Also clear existing contours.
      open_cubes_

      if { $match_ } {
         #  Connection should go through the domain of the current coordinates.
         set domain [gaiautils::astget $target_wcs "Domain"]

         #  Locate the DSBSpecFrame or SpecFrame, if we have any.
         set dsbspecaxis_(target) [locate_axis_ $target_wcs "dsbspecframe"]
         set specaxis_(target) [locate_axis_ $target_wcs "specframe"]

         #  Set the AlignSideBand attribute as required.
         if { $dsbspecaxis_(target) != 0 } {
            gaiautils::astset $target_wcs \
               "AlignSideBand($dsbspecaxis_(target))=$match_sidebands_"
         }

         #  If matching using the spectral base system, set it (needed to
         #  match velocities with different rest frequencies).
         if { $specaxis_(target) != 0 && $match_base_system_ } {
            set target_system \
               [gaiautils::astget $target_wcs "system($specaxis_(target))"]
            gaiautils::astset $target_wcs \
               "AlignSystem($specaxis_(target))=$target_system"
         }

         #  The relevant coordinates are base to base as we are working in
         #  graphics coordinates, same as GRID. So switch this WCS to that
         #  domain. Same applies to the other WCS.
         set target_current [gaiautils::astget $target_wcs "Current"]
         set target_base [gaiautils::astget $target_wcs "Base"]
         gaiautils::astset $target_wcs "Current=$target_base"

         #  Don't want to exit this method without resetting the domain.
         catch {
            for { set i 0 } { $i < $itk_option(-maxcubes) } { incr i } {
               if { $display_($i) &&
                    [info exists cubename_($i)] && $cubename_($i) != {} } {

                  #  If required match the spectral coordinates using
                  #  sidebands. Both need setting and have DSBSpecFrames.
                  if { $dsbspecaxis_(target) != 0 && $dsbspecaxis_($i) != 0 } {
                     gaiautils::astset $wcs_($i) \
                        "AlignSideBand($dsbspecaxis_($i))=$match_sidebands_"
                  }

                  #  Connect the coordinates of this cube with the target.
                  set wcs_current [gaiautils::astget $wcs_($i) "Current"]
                  set wcs_base [gaiautils::astget $wcs_($i) "Base"]
                  gaiautils::astset $wcs_($i) "Current=$wcs_base"

                  set connection_wcs \
                     [gaiautils::astconvert $wcs_($i) $target_wcs $domain]
                  gaiautils::astset $wcs_($i) "Current=$wcs_current"
                  gaiautils::astset $wcs_($i) "Base=$wcs_base"

                  #  Create the segmenter for this mask.
                  create_segmenter_ $i $connection_wcs $renwindow

                  #  Add all the required levels and lookup table.
                  update_segmenter_ $i
               }
            }
         } msg
         if { $msg != {} } {
            puts stderr "Error matching cube coordinate systems: $msg"
         }

         #  Back to original base and current frames.
         gaiautils::astset $target_wcs "Current=$target_current"
         gaiautils::astset $target_wcs "Base=$target_base"

         #  Clear AlignSystem if set.
         if { $specaxis_(target) != 0 && $match_base_system_ } {
            gaiautils::astclear $target_wcs "AlignSystem($specaxis_(target))"
         }

      } else {
         #  Not matching. Transforms are unit maps.
         for { set i 0 } { $i < $itk_option(-maxcubes) } { incr i } {
            if { $display_($i) && [info exists cubename_($i)] && $cubename_($i) != {} } {
               create_segmenter_ $i {} $renwindow
               update_segmenter_ $i
            }
         }
      }
   }

   #  Access all active cubes and wrap these into instances of vtkArrayData.
   protected method open_cubes_ {} {
      for { set i 0 } { $i < $itk_option(-maxcubes) } { incr i } {
         if { [info exists cubename_($i)] && $cubename_($i) != {} } {

            #  Check name of cube data, if changed need to release the
            #  existing cube and access the new data.
            set newname [$cubeaccessor_($i) cget -dataset]
            if { $cubename_($i) != $newname } {

               #  New cube or data has changed. Release existing cube and data.
               if { [info exists imagedata_($i)] && $imagedata_($i) != {} } {
                  release_cube_ $i
               }

               #  And access the new cube.
               $cubeaccessor_($i) configure -dataset $cubename_($i)
               $itk_component(cube$i) add_to_history $cubename_($i)

               #  For speed of access cache the full WCS.
               set wcs_($i) [$cubeaccessor_($i) getwcs]

               #  And check for SpecFrames and DSBSpecFrames.
               set dsbspecaxis_($i) [locate_axis_ $wcs_($i) "dsbspecframe"]
               set specaxis_($i) [locate_axis_ $wcs_($i) "specframe"]

               #  Access data, wrapping to an vtkImageData instance.
               set imagedata_($i) [gaia3d::Gaia3dVtkCubeData \#auto]
               $imagedata_($i) configure -cubeaccessor $cubeaccessor_($i) \
                  -checkbad 0 -nullvalue 0
               $imagedata_($i) access
            }
         }

         #  Always release objects.
         release_objects $i
      }
   }

   #  Create objects to segment the mask into regions based on their data
   #  value: i is cube index, "wcs" is an AST Mapping/FrameSet connecting the
   #  cube coordinates to the graphics coordinates, can be {}.
   protected method create_segmenter_ {i wcs renwindow} {
      if { $cubename_($i) != {} } {
         if { $wcs != {} } {
            set segmenter_($i) [Gaia3dVtkSegmenter \#auto \
                                   -imagedata [$imagedata_($i) get_imagedata] \
                                   -renwindow $renwindow -wcs $wcs]
         } else {
            set segmenter($i) [Gaia3dVtkSegmenter \#auto \
                                   -imagedata [$imagedata_($i) get_imagedata] \
                                   -renwindow $renwindow]
         }
         $segmenter_($i) add_to_window
         $segmenter_($i) set_visible
      }
   }

   #  Update the objects managing a mask. The values are gathered from the
   #  interface.
   protected method update_segmenter_ {i} {

      #  Do nothing if the contour actors don't exist.
      if { ! [info exists segmenter_($i)] } {
         return
      }

      #  Get and set attributes.
      $segmenter_($i) set_lut [get_lut_ $i] [get_opacity_ $i]
      $segmenter_($i) set_values [get_values_ $i]
   }

   #  Get the values to be segmented from a mask.
   protected  method get_values_ {i} {

      if { $values_($i) == {} } {
         #  All.
         return -1
      }

      #  Need lists of single values or pairs of values.
      #  Clean up first, replace "," with space, remove multiple spaces and trim.
      set value [regsub -all -- {,} $values_($i) { }]
      set value [regsub -all -- {\s+} $value { }]
      set value [string trim $value]
      set range {}
      foreach v $value {
         #  Ranges are low-high, that's a single element.
         lappend range [regsub -all -- {-} $v { }]
      }
      return $range
   }

   #  Get which lookup table to use for colouring segments.
   protected method get_lut_ {i} {
      return $lut_($i)
   }

   #  Set which lookup table to use for colouring segments.
   protected method set_lut_ {i value} {
      set lut_($i) $value
   }

   #  Get the opacity of the colours.
   protected method get_opacity_ {i} {
      return $opacity_($i)
   }

   #  Set the opacity of the colours.
   protected method set_opacity_ {i value} {
      set opacity_($i) $value
   }

   #  Release all segmenters.
   public method release_all_objects {} {
      if { [info exists segmenter_] } {
         for {set i 0} {$i < $itk_option(-maxcubes)} {incr i} {
            release_objects $i
         }
         unset segmenter_
      }
   }

   #  Release segmenter for a cube.
   public method release_objects {i} {
      if { [info exists segmenter_($i)] } {
         catch {
            $segmenter_($i) remove_from_window
            ::delete object $segmenter_($i)
            unset segmenter_($i,$j)
         }
      }
   }

   #  Add main controls for selecting cubes and defining the attributes for
   #  rendering.
   private method add_controls_ {} {

      #  Plain decorations if just one cube, other sits in a scrolledframe.
      if { $itk_option(-maxcubes) != 1 } {
         itk_component add scrollframe {
            ::iwidgets::scrolledframe $w_.scrollframe \
               -height 200 \
               -hscrollmode dynamic \
               -vscrollmode dynamic
         }
         pack $itk_component(scrollframe) -fill both -expand 1
         set parent [$itk_component(scrollframe) childsite]

         #  Work hard to get a width to the childsite, boy...
         set canvas [winfo parent $parent]
         $canvas itemconfigure frameTag -width 350
         set orient horizontal
      } else {
         set parent $w_
         set orient vertical
      }

      #  Add controls for selecting cubes, and defining their attributes.
      for { set i 0 } { $i < $itk_option(-maxcubes) } { incr i } {

         if { $itk_option(-maxcubes) != 1 } {
            itk_component add attrule$i {
               LabelRule $parent.attrule$i \
                  -text "Mask & attributes:"
            }
            pack $itk_component(attrule$i) -side top -fill x -expand 1
         }

         #  Create cubeaccessor.
         set cubeaccessor_($i) [uplevel \#0 gaia::GaiaNDAccess \#auto]

         #  Display or not.
         set display_($i) 1
         itk_component add display$i {
            gaia::StarLabelCheck $parent.display$i \
               -text "Display:" \
               -onvalue 1 \
               -offvalue 0 \
               -labelwidth $lwidth_ \
               -anchor w \
               -variable [scope display_($i)]
         }
         pack $itk_component(display$i) -side top -fill x -ipadx 1m
         add_short_help $itk_component(display$i) \
            {Display cube in rendered scene}

         #  Select cube.
         set cubename_($i) {}
         itk_component add cube$i {
            LabelCubeFileChooser $parent.cube$i \
               -labelwidth $lwidth_ \
               -text "Mask:" \
               -textvariable [scope cubename_($i)] \
               -command [code $this set_cubename_ $i] \
               -filter_types $itk_option(-filter_types) \
               -chooser_title "Select CUPID mask" \
               -cubeaccessor $cubeaccessor_($i) \
               -orientation $orient
         }
         pack $itk_component(cube$i) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(cube$i) \
            {Name of a data file, must be a cube}

         #  Which regions to display? All, selected (from catalogue?) or a given
         #  list.
         itk_component add values$i {
            util::LabelEntry $parent.values$i \
               -labelwidth $lwidth_ \
               -text "Values:" \
               -textvariable [scope values_($i)]
         }
         pack $itk_component(values$i) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(values$i) \
            {Values of masked regions to display, empty for all}

         #  Lookup table selection. Very basic for these.
         itk_component add lut$i {
            util::LabelMenu $parent.lut$i \
               -text "Colours:" \
               -labelwidth $lwidth_ \
               -relief raised
         }
         foreach {index desc} "1 {Random colour} 2 {Rainbow colour} 3 {Random grey}" {
            $itk_component(lut$i) add \
               -label $desc \
               -value $index \
               -command [code $this set_lut_ $i $index]
         }
         $itk_component(lut$i) configure -value 1
         set lut_($i) 1
         pack $itk_component(lut$i) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(lut$i) \
            {Lookup table for assigning colours to regions}

         #  Add menu for selecting the opacity.
         itk_component add opacity$i {
            util::LabelMenu $parent.opacity$i \
               -text "Opacity:" \
               -labelwidth $lwidth_ \
               -relief raised
         }

         #  Now add the range of opacities to it.
         for {set j 0.0} {$j <= 1.0} {set j [expr $j+0.1]} {
            $itk_component(opacity$i) add \
               -label $j \
               -value $j \
               -command [code $this set_opacity_ $i $j]
         }
         $itk_component(opacity$i) configure -value 1.0
         set opacity_($i) 1.0
         pack $itk_component(opacity$i) -side top -fill x -ipadx 1m -ipady 2m
         add_short_help $itk_component(opacity$i) \
            {Opacity of coloured regions}
      }
   }

   #  Set the name of a cube.
   protected method set_cubename_ {i name} {
      set cubename_($i) $name
   }

   #  Locate an axis of a given type in the given cube WCS.
   protected method locate_axis_ {wcs type} {
      set axis 0
      for {set i 1} {$i < 4} {incr i} {
         if { [gaiautils::frameisa $wcs $i $type] } {
            set axis $i
            break
         }
      }
      return $axis
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  Maximum number of additional cubes.
   itk_option define -maxcubes maxcubes MaxCubes 1

   #  Filters for selecting files.
   itk_option define -filter_types filter_types Filter_types {}

   #  Menu to populate with options for matching coordinate systems.
   itk_option define -options_menu options_menu Options_Menu {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Names of the cubes, index by integer (0->maxcubes-1).
   protected variable cubename_

   #  The cube accessors.
   protected variable cubeaccessor_

   #  The cached AST FrameSet.
   protected variable wcs_

   #  Indices of the SpecFrame and DSBSpecFrame axes, if present.
   protected variable specaxis_
   protected variable dsbspecaxis_

   #  Gaia3dVtkCubeData instances. Provide access to wrapped cube.
   protected variable imagedata_

   #  Segmenter instances.
   protected variable segmenter_

   #  Whether named cube should be rendered.
   protected variable display_

   #  Whether to match the cube coordinate systems.
   protected variable match_ 1

   #  Whether to match the cube spectral coordinates using sideband knowledge.
   protected variable match_sidebands_ 0

   #  Whether to match the cube spectral coordinates using the base system.
   protected variable match_base_system_ 1

   #  Values of masked regions to display.
   protected variable values_

   #  Lookup table for each segmenter
   protected variable lut_

   #  Opacity for each segmenter.
   protected variable opacity_

   #  Usual labelwith.
   protected variable lwidth_ 7

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}

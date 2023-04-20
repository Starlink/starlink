#+
#  Name:
#     Gaia3dVolume

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class for displaying a volume rendering of a data cube.

#  Description:
#     This class renders a data cube as a volume using the facilities of the
#     VTK library.

#  Invocations:
#
#        Gaia3dVolume object_name [configuration options]
#
#     This creates an instance of a Gaia3dVolume object. The return is
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
#     03-JUL-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual Gaia3dVolume {}

itcl::class gaia3d::Gaia3dVolume {

   #  Inheritances:
   #  -------------
   inherit gaia3d::Gaia3dTool

   #  Constructor:
   #  ------------
   constructor {args} {

      #  We use BAD pixel replacement by default.
      set checkbad_ 1

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA3D: Volume render ($itk_option(-number))"

      #  Make it a decent size (packing doesn't work).
      wm geometry  $w_ 800x800+0+0

      #  Add window help.
      add_help_button volume "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Add an option control if rendering is done at high resolution.
      set Options [get_menu Options]
      $Options add checkbutton -label "High resolution" \
         -variable [scope itk_option(-high_resolution)] \
         -onvalue 1 \
         -offvalue 0
      add_menu_short_help $Options {High resolution}  \
         {Render volume using high resolution to reduce artifacts}
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Min and max colour menus.
      if { $min_colour_menu_ != {} } {
         catch {delete object $min_colour_menu_}
      }
      if { $max_colour_menu_ != {} } {
         catch {delete object $max_colour_menu_}
      }
   }

   #  Methods:
   #  --------

   #  Add the controls for the volume attributes.
   protected method add_tool_controls_ {} {

      #  Get pane for attributes.
      $itk_component(tab) add -label Volume

      #  Add controls for line attributes.
      set w [$itk_component(tab) childsite 0]
      set lwidth 10

      #  Option to use a min/max colour transfer function or one based on a
      #  standard colour map.
      itk_component add coloursource {
         gaia::StarLabelCheck $w.coloursource \
            -text "Two colour:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope twocolour_source_] \
            -command [code $this changed_colour_source_]
      }
      pack $itk_component(coloursource) -fill x -expand 0 -ipady 5
      add_short_help $itk_component(coloursource) \
         {Use two colours or a standard colour map to colour volume}

      #  Menus for selecting the min and max colours.
      itk_component add mincolour {
         util::LabelMenu $w.mincolour \
            -relief raised \
            -labelwidth $lwidth \
            -text "Min colour:" \
            -orient vertical
      }
      add_short_help $itk_component(mincolour) {Colour for Min value}

      itk_component add maxcolour {
         util::LabelMenu $w.maxcolour \
            -relief raised \
            -labelwidth $lwidth \
            -text "Max colour:" \
            -orient vertical
      }
      add_short_help $itk_component(maxcolour) {Colour for Max value}

      #  Menu of colour maps.
      itk_component add colourmap {
         util::LabelMenu $w.colourmap \
            -relief raised \
            -labelwidth $lwidth \
            -text "Colour map:" \
            -orient vertical
      }
      set n 0
      foreach {cmap name} [get_colourmaps] {
         if { $n > 20 } {
            set cbreak 1
            set n 1
         } else {
            set cbreak 0
            incr n
         }
         $itk_component(colourmap) add \
            -label $name \
            -value $cmap \
            -variable [scope volume_colourmap_] \
            -columnbreak $cbreak \
            -command [code $this changed_colourmap_ $cmap]
      }
      $itk_component(colourmap) configure -value $volume_colourmap_

      #  Now add all the colours.
      set min_colour_menu_ [gaia::ColourLabelMenu \#auto \
                               $itk_component(mincolour) \
                               -show_custom 0]
      set max_colour_menu_ [gaia::ColourLabelMenu \#auto \
                               $itk_component(maxcolour) \
                               -show_custom 0]

      #  Set default colours, black and orange to match GAIA.
      $itk_component(mincolour) configure \
         -value [gaia::AstColours::lookup_colour 1]
      $itk_component(maxcolour) configure \
         -value [gaia::AstColours::lookup_colour 8]

      pack $itk_component(mincolour) -fill x -expand 0
      pack $itk_component(maxcolour) -fill x -expand 0
      pack $itk_component(colourmap) -fill x -expand 0

      #  Disable/enable these as needed.
      changed_colour_source_

      #  Data limits.
      itk_component add minvalue {
         util::LabelEntry $w.minvalue \
            -text "Min value:" \
            -labelwidth $lwidth \
            -validate real \
            -orient vertical
      }
      add_short_help $itk_component(minvalue) \
         {Minimum data value used during rendering}

      itk_component add maxvalue {
         util::LabelEntry $w.maxvalue \
            -text "Max value:" \
            -labelwidth $lwidth \
            -validate real \
            -orient vertical
      }
      add_short_help $itk_component(maxvalue) \
         {Maximum data value used during rendering}

      pack $itk_component(minvalue) -fill x -expand 0
      pack $itk_component(maxvalue) -fill x -expand 0

      #  Set initial data limits, use upper quartile portion of data range.
      if { $itk_option(-rtdimage) != {} } {
         lassign [$itk_option(-rtdimage) cut] cmin cmax
         set max [$itk_option(-rtdimage) max]

         $itk_option(-rtdimage) autocut -percent 75
         lassign [$itk_option(-rtdimage) cut] l75 h75

         $itk_component(minvalue) configure -value $h75
         $itk_component(maxvalue) configure -value $max

         $itk_option(-rtdimage) cut $cmin $cmax
      } else {
         $itk_component(minvalue) configure -value 0.0
         $itk_component(maxvalue) configure -value 0.0
      }

      #  Opacity limits.
      itk_component add minopacity {
         util::LabelEntry $w.minopacity \
            -text "Min opacity:" \
            -labelwidth $lwidth \
            -validate real \
            -orient vertical
      }
      add_short_help $itk_component(minopacity) \
         {Minimum opacity used during rendering (min value)}

      itk_component add maxopacity {
         util::LabelEntry $w.maxopacity \
            -text "Max opacity:" \
            -labelwidth $lwidth \
            -validate real \
            -orient vertical
      }
      add_short_help $itk_component(maxopacity) \
         {Maximum opacity used during rendering (max value)}

      pack $itk_component(minopacity) -fill x -expand 0
      pack $itk_component(maxopacity) -fill x -expand 0

      $itk_component(minopacity) configure -value 0.0
      $itk_component(maxopacity) configure -value 0.5

      #  Add a button to stop the rendering.
      itk_component add stop {
         button $itk_component(actionframe).stop -text {Stop} \
            -command [code $this stop]
      }
      add_short_help $itk_component(stop) {Stop rendering}
      pack $itk_component(stop) -side left -expand 1 -pady 3 -padx 3

      #  Display the pane.
      $itk_component(tab) select 0
   }

   #  Add a customized colour to the menus. Use an index if supplied.
   #  Otherwise create one.
   public method add_custom_colour { new_colour {index -1} } {
      if { $index == -1 } {
         #  Slight trick here is to get a valid index first and then use
         #  it for all additional entries
         set index [$min_colour_menu_ add_custom_colour $new_colour $index]
         $max_colour_menu_ add_custom_colour $new_colour $index
      } else {
         $min_colour_menu_ add_custom_colour $new_colour $index
         $max_colour_menu_ add_custom_colour $new_colour $index
      }
      gaia3d::Gaia3dTool::add_custom_colour $new_colour $index
   }

   #  Called when the colour source is changed. Need to act on this. Note
   #  render only when "Draw" is pressed.
   protected method changed_colour_source_ {} {
      if { $twocolour_source_ } {
         $itk_component(mincolour) configure -state normal
         $itk_component(maxcolour) configure -state normal
         $itk_component(colourmap) configure -state disabled
      } else {
         $itk_component(mincolour) configure -state disabled
         $itk_component(maxcolour) configure -state disabled
         $itk_component(colourmap) configure -state normal
      }
   }

   #  Called when the colour map is changed.
   protected method changed_colourmap_ {value} {
      set volume_colourmap_ $value
   }

   #================================================================
   #  VTK setup.
   #================================================================

   #  Render the volume. If data has changed newdata is set and the
   #  scene has been cleared.
   protected method draw_scene_ { newdata } {

      #  Create the scene.
      set reset_camera 0
      if { $volume_ == {} } {
         set newdata 1
         create_scene_
         set reset_camera 1
      }

      #  Update the scene to use the current settings.
      update_scene_ $newdata

      #  Reset camera, note need to do this for new volume, not just new data.
      #  New data reset performed in super-class.
      if { $reset_camera } {
         $renwindow_ reset_camera
      }
   }

   #  Create the various props, renderers etc. for the scene.
   protected method create_scene_ {} {

      #  Create mapper that handles any data type. This does the hard work.
      set mainmapper_ [::vtkFixedPointVolumeRayCastMapper New]

      #  Add a second mapper with course detailing for interactive updates.
      set fastmapper_ [::vtkFixedPointVolumeRayCastMapper New]

      #  Don't do threaded work, that isn't safe with GAIA.
      $mainmapper_ SetNumberOfThreads 0
      $fastmapper_ SetNumberOfThreads 0

      #  Course mapper should be adaptive.
      $fastmapper_ AutoAdjustSampleDistancesOn

      #  Observe the progress of the mappers so that we can update the UI
      #  during long rendering jobs.
      $mainmapper_ AddObserver ProgressEvent \
         [code $this report_mapper_progress_ $mainmapper_]
      $fastmapper_ AddObserver ProgressEvent \
         [code $this report_mapper_progress_ $fastmapper_]

      #  Opacity and colour functions.
      set colourfunction_ [::vtkColorTransferFunction New]
      set opacityfunction_ [::vtkPiecewiseFunction New]

      #  Assign these to a properties for the two mappers.
      set mainproperty [::vtkVolumeProperty New]
      $mainproperty SetColor $colourfunction_
      $mainproperty SetScalarOpacity $opacityfunction_

      set fastproperty [::vtkVolumeProperty New]
      $fastproperty SetColor $colourfunction_
      $fastproperty SetScalarOpacity $opacityfunction_

      #  Use linear interpolation for full renderer, fast uses nearest
      #  neighbour.
      $mainproperty SetInterpolationTypeToLinear
      $fastproperty SetInterpolationTypeToNearest

      #  Populate a LOD actor with our two mappers.
      set volume_ [::vtkLODProp3D New]
      set l1 [$volume_ AddLOD $mainmapper_ $mainproperty 0.0]
      $volume_ AddLOD $fastmapper_ $fastproperty 0.0
      $mainproperty Delete
      $fastproperty Delete

      #  Add for rendering as part of the scene.
      $renwindow_ add_view_prop $volume_
   }

   #  Update the scene to use the given settings.
   protected method update_scene_ {newdata} {

      #  Set image data, if changed or new.
      if { $newdata } {
         set stencil [$imagedata_ get_stencil]
         if { $stencil == {} } {
            $mainmapper_ SetInputData [$imagedata_ get_imagedata]
            $fastmapper_ SetInputData [$imagedata_ get_imagedata]
         } else {
            $mainmapper_ SetInputConnection [$stencil GetOutputPort]
            $fastmapper_ SetInputConnection [$stencil GetOutputPort]
         }
      }

      #  Gather properties.
      set mincolour [$itk_component(mincolour) get]
      set maxcolour [$itk_component(maxcolour) get]
      set minval [$itk_component(minvalue) get]
      set maxval [$itk_component(maxvalue) get]
      set minop [$itk_component(minopacity) get]
      set maxop [$itk_component(maxopacity) get]

      #  Main mapper to full precision if requested.
      if { $itk_option(-high_resolution) } {
         $mainmapper_ AutoAdjustSampleDistancesOff
         $mainmapper_ SetSampleDistance 0.25
      } else {
         $mainmapper_ AutoAdjustSampleDistancesOn
         $mainmapper_ SetSampleDistance 1.0
      }

      #  Need to assign opacities and colours on the basis of data values.
      #  The colours are either the range between two colours, or based on
      #  a colourmap.
      $colourfunction_ RemoveAllPoints
      if { $twocolour_source_ } {
         $colourfunction_ SetRange $minval $maxval
         lassign [::gaia3d::Gaia3dVtkWindow::get_rgb_colour $mincolour] R G B
         $colourfunction_ AddRGBPoint $minval $R $G $B
         lassign [::gaia3d::Gaia3dVtkWindow::get_rgb_colour $maxcolour] R G B
         $colourfunction_ AddRGBPoint $maxval $R $G $B
      } else {
         gvtk::cmap set $volume_colourmap_ $colourfunction_ $minval $maxval
         $colourfunction_ SetRange $minval $maxval
      }

      #  Opacity.
      $opacityfunction_ RemoveAllPoints
      $opacityfunction_ AddPoint $minval $minop
      $opacityfunction_ AddPoint $maxval $maxop

      #  Make sure rendering will update (volume not changed directly).
      $volume_ Modified
   }

   protected method report_mapper_progress_ {mapper} {
      #  puts "Volume rendering: [expr [$mapper GetProgress]*100.0] percent"
      ::update idletasks
   }

   #  Let the vtkLODProp3D choose the detail.
   protected method set_lod_auto_ {} {
      $volume_ SetAutomaticLODSectionOn
   }

   #  Release all scene objects. Also releases the imagedata, if full
   #  release is true.
   public method release_objects { fullrelease } {
      if { $volume_ != {} } {

         #  Stop rendering volume.
         $renwindow_ remove_view_prop $volume_

         #  Now delete the volume and related objects.
         $volume_ Delete
         set volume_ {}

         $mainmapper_ Delete
         set mainmapper_ {}

         $fastmapper_ Delete
         set fastmapper_ {}

         $colourfunction_ Delete
         set colourfunction_ {}

         $opacityfunction_ Delete
         set opacityfunction_ {}
      }
      gaia3d::Gaia3dTool::release_objects $fullrelease
   }

   #  Stop rendering.
   public method stop {} {
      $renwindow_ stop
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  If using high resolution when rendering.
   itk_option define -high_resolution high_resolution High_Resolution 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  VTK variables:
   protected variable volume_ {}
   protected variable mainmapper_ {}
   protected variable fastmapper_ {}
   protected variable opacityfunction_ {}
   protected variable colourfunction_ {}
   protected variable lookup_ {}

   #  Type of colour transfer function. Two-colour by default.
   protected variable twocolour_source_ 1

   #  Colourmap used, if not two colour.
   protected variable volume_colourmap_ "real.lasc"

   #  Min and max colour menus.
   protected variable min_colour_menu_ {}
   protected variable max_colour_menu_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}

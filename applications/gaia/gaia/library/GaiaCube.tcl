#+
#  Name:
#     GaiaCube

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Control the display of a cube as a series of images in GAIA.

#  Description:
#     This class opens a 3D data cube and then displays image shaped sections
#     of it in an associated window.

#  Invocations:
#
#        GaiaCube object_name [configuration options]
#
#     This creates an instance of a GaiaCube object. The return is
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
#     See itk_option definitions below.

#  Methods:
#     See individual method declarations below.

#  Inheritance:
#     util::TopLevelWidget

#  Copyright:
#     Copyright (C) 2004-2005 Central Laboratory of the Research Councils.
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     08-OCT-2004 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCube {}

itcl::class gaia::GaiaCube {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      set lwidth 16
      set vwidth 10

      #  Set window properties.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]
      wm title $w_ "Display image sections of a cube ($itk_option(-number))"

      #  Add short help window
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File" left]
      configure_menubutton File -underline 0

      #  Add the close menu item.
      $File add command -label Close \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      #  Add the Options menu.
      set Options [add_menubutton "Options" left]
      configure_menubutton Options -underline 0

      #  If HDS file mapping is not available, then set value off.
      set canmmap 1
      if { [hds::gtune MAP] != "1" } {
         configure -usemmap 0
         set canmmap 0
      }

      #  Whether to mmap cube or load directly into memory, usually
      #  mmap'd. Control-l forces direct load, if not already done.
      $Options add checkbutton -label "Memory map cube data" \
         -command [code $this load_cube_ 0] \
         -accelerator {Control-l} \
         -variable [scope itk_option(-usemmap)] \
         -onvalue 1 \
         -offvalue 0
      bind $w_ <Control-l> [code $this configure -usemmap 1]
      add_menu_short_help $Options {Memory map cube data}  \
         {Access cube data using mmap(), otherwise read fully into RAM}

      #  If memory mapping not available, then also disable this option.
      if { ! $canmmap } {
         $Options entryconfigure {Memory map cube data} -state disabled
         set itk_option(-usemmap) 0
      }

      #  Whether to constantly update the image cuts.
      $Options add checkbutton -label "Autocut" \
         -variable [scope itk_option(-autocut)] \
         -onvalue 1 \
         -offvalue 0
      add_menu_short_help $Options {Autocut}  \
         {Continuously change set the cuts of the image slices, data limits}

      #  Enable send to SPLAT, this overrides the default double-click which
      #  defines a reference spectrum.
      $Options add checkbutton -label "Enable SPLAT" \
         -command [code $this set_splat_bindings_] \
         -variable [scope itk_option(-use_splat)] \
         -onvalue 1 \
         -offvalue 0
      add_menu_short_help $Options {Enable SPLAT}  \
         {Make double click send to SPLAT}

      #  Add the coordinate selection menu.
      set SpectralCoords [add_menubutton "Coords" left]
      configure_menubutton "Coords" -underline 0
      set spec_coords_ [GaiaSpecCoords \#auto \
                           -change_cmd [code $this coords_changed_]]
      $spec_coords_ add_menu $SpectralCoords

      #  Add window help.
      add_help_button cube "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Name of input dataset.
      itk_component add cube {
         LabelFileChooser $w_.cube \
            -labelwidth $lwidth \
            -text "Input cube:" \
            -textvariable [scope itk_option(-cube)] \
            -command [code $this set_chosen_cube_] \
            -filter_types $itk_option(-filter_types)
      }
      pack $itk_component(cube) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(cube) \
         {Name of the input data file, must be a cube}

      #  Control for selecting the axis we move along.
      itk_component add axis {
         LabelMenu $w_.axis \
            -text "Axis:" \
            -labelwidth $lwidth
      }
      foreach {label value} { one 1 two 2 three 3 } {
         $itk_component(axis) add \
            -command [code $this set_step_axis_ $value] \
            -label $label \
            -value $value
      }
      $itk_component(axis) configure -value 3

      pack $itk_component(axis) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(axis) \
         {Axis to step along}

      #  Slider that moves along the chosen axis.
      itk_component add index {
         GaiaSpectralPlotLine $w_.index \
            -gaiacube [code $this] \
            -ref_id 1 \
            -text {Index of plane:} \
            -value $plane_ \
            -show_type 1 \
            -show_ref_line 1 \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -coord_update_cmd [code $this set_display_plane] \
            -drag_update_cmd [code $this update_wcs]
      }
      set ref_line_controls_(1) $itk_component(index)
      pack $itk_component(index) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(index) \
         {Index of the image plane to display (along current axis)}

      #  Add tab window for choosing either the helper controls.
      itk_component add tabnotebook {
         iwidgets::tabnotebook $w_.tab \
            -angle 0 -tabpos n -width 400 -height 400
      }
      pack $itk_component(tabnotebook) -fill both -expand 1

      $itk_component(tabnotebook) add -label Spectrum
      set spectrumTab [$itk_component(tabnotebook) childsite 0]

      $itk_component(tabnotebook) add -label Animation
      set animationTab [$itk_component(tabnotebook) childsite 1]

      $itk_component(tabnotebook) add -label Collapse
      set collapseTab [$itk_component(tabnotebook) childsite 2]

      $itk_component(tabnotebook) add -label Chanmap
      set chanmapTab [$itk_component(tabnotebook) childsite 3]

      $itk_component(tabnotebook) add -label Baseline
      set baselineTab [$itk_component(tabnotebook) childsite 4]

      #  Spectrum section.
      itk_component add sruler {
         LabelRule $spectrumTab.sruler -text "Spectrum controls:"
      }
      pack $itk_component(sruler) -side top -fill x

      set ref_ids 1
      itk_component add spectrum {
         GaiaCubeSpectrum $spectrumTab.spectrum \
            -gaiacube [code $this] \
            -gaia $itk_option(-gaia) \
            -ref_id $ref_ids \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
      }
      set ref_range_controls_($ref_ids) $itk_component(spectrum)
      pack $itk_component(spectrum) -side top -fill both -ipadx 1m -ipady 2m

      #  Animation section.

      itk_component add aruler {
         LabelRule $animationTab.aruler -text "Animation controls:"
      }
      pack $itk_component(aruler) -side top -fill x

      incr ref_ids
      itk_component add animation {
         GaiaCubeAnimation $animationTab.animation \
            -gaiacube [code $this] \
            -ref_id $ref_ids \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth
      }
      set ref_range_controls_($ref_ids) $itk_component(animation)
      pack $itk_component(animation) -side top -fill both -ipadx 1m -ipady 2m

      #  Collapse section.

      itk_component add cruler {
         LabelRule $collapseTab.cruler -text "Collapse controls:"
      }
      pack $itk_component(cruler) -side top -fill x

      incr ref_ids
      itk_component add collapse {
         GaiaCubeCollapse $collapseTab.collapse \
            -gaiacube [code $this] \
            -ref_id $ref_ids \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth
      }
      set ref_range_controls_($ref_ids) $itk_component(collapse)
      pack $itk_component(collapse) -side top -fill both -ipadx 1m -ipady 2m

      #  Chanmap section.

      itk_component add chanmapruler {
         LabelRule $chanmapTab.chanmapruler -text "Channel map controls:"
      }
      pack $itk_component(chanmapruler) -side top -fill x

      incr ref_ids
      itk_component add chanmap {
         GaiaCubeChanmap $chanmapTab.chanmap \
            -gaiacube [code $this] \
            -ref_id $ref_ids \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth
      }
      set ref_range_controls_($ref_ids) $itk_component(chanmap)
      pack $itk_component(chanmap) -side top -fill both -ipadx 1m -ipady 2m

      #  Baseline subtraction section. Must be the last.

      itk_component add baselineruler {
         LabelRule $baselineTab.baselineruler \
            -text "Baseline subtraction controls:"
      }
      pack $itk_component(baselineruler) -side top -fill x

      set baseline_id_ [incr ref_ids]
      itk_component add baseline {
         GaiaCubeBaseline $baselineTab.baseline \
            -gaiacube [code $this] \
            -ref_id $baseline_id_ \
            -lower_limit $plane_ \
            -upper_limit $plane_ \
            -show_ref_range 0 \
            -labelwidth $lwidth \
            -valuewidth $vwidth
      }
      set ref_range_controls_($baseline_id_) $itk_component(baseline)
      pack $itk_component(baseline) -side top -fill both -expand 1 \
         -ipadx 1m -ipady 2m

      #  Close window.
      itk_component add close {
         button $w_.close -text Close \
            -command [code $this close]
      }
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(close) {Close window}

      #  Reveal first page of controls.
      $itk_component(tabnotebook) select 0
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Close and release the cube.
      if { $cubeaccessor_ != {} } {
         $cubeaccessor_ close
         set cubeaccessor_ {}
      }

      #  Delete the NDF image slice.
      if { $section_name_ != {} } {
         catch {::file delete $section_name_} msg
      }

      #  Delete any registered temporary files.
      foreach filename $temp_files_ {
         if { [file exists $filename] } {
            catch {file delete $filename}
         }
      }

      #  Close spectrum plot, also removes any graphics and bindings.
      $itk_component(spectrum) close

      #  Halt any animations.
      $itk_component(animation) stop
   }

   #  Methods:
   #  --------

   #  Close window.
   public method close {} {
      wm withdraw $w_

      #  Close spectrum plot.
      $itk_component(spectrum) close

      #  Halt any animations.
      $itk_component(animation) stop
   }

   #  Undo some of the effects of close. Do not reopen cube, that would 
   #  be expensive and reset too much.
   public method open {} {
      $itk_component(spectrum) open
   }

   #  Open the chosen file as a cube.
   protected method set_chosen_cube_ { args } {
      set namer [GaiaImageName \#auto -imagename $itk_option(-cube)]
      if { [$namer exists] } {
         if { $cubeaccessor_ == {} } {
            set cubeaccessor_ [uplevel \#0 GaiaNDAccess \#auto]
         }

         $namer absolute
         set ndfname_ [$namer ndfname 0]
         $cubeaccessor_ configure -dataset "$ndfname_"
         set bounds_ [$cubeaccessor_ getbounds 0]
         set ndims [expr [llength $bounds_]/2]

         #  Allow fourth dimension, as long as it is redundant.
         if { $ndims == 4 } {
            if { [expr [lindex $bounds_ 7] - [lindex $bounds_ 6]] == 0 } {
               set close_section_ ",1)"
               set bounds_ [lrange $bounds_ 0 5]
               set ndims 3
            }
         } else {
            #  Sections just close with parenthesis.
            set close_section_ ")"
         }
         if { $ndims != 3 } {
            error_dialog \
               "Not a cube, must have 3 significant dimensions (has $ndims)"
            return
         }

         #  Map in all data, this makes it immediately available within
         #  application
         load_cube_ 1

         #  Retain the object value for creating new ones.
         set object_ [$itk_option(-rtdimage) object]

         #  Set axis and display the plane for first time.
         set axis_ 2
         $itk_component(axis) configure -value 3
         set_step_axis_ 3

         #  Set up object to control units.
         $spec_coords_ configure -axis 3 -accessor $cubeaccessor_
      }
   }

   #  Load the cube data, using the current mmap mode. Only happens when force
   #  is true, or if a change the mmap mode of the cube is needed.
   #  Controlling the mmap mode can force file i/o to be is used to load all
   #  the cube into malloc'd memory, which speeds the spectral
   #  readout. Otherwise mmap is used, which may give an initially slow
   #  spectral readout for large cubes.
   protected method load_cube_ { force } {
      if { $cubeaccessor_ == {} } {
         return
      }
      if { [$cubeaccessor_ cget -usemmap] != $itk_option(-usemmap) || $force } {
         busy {
            $cubeaccessor_ unmap
            $cubeaccessor_ configure -usemmap $itk_option(-usemmap)
            set adr [$cubeaccessor_ map "READ"]
         }
      }
   }

   #  Set the axis we step along.
   #
   #  If setplane is true (the default) then a side-effect of this is to
   #  create the dummy NDF that will be actually manipulated in the main
   #  display window and position on the central plane. A dummy NDF is used
   #  for FITS and NDF files as this is the simplest way to make sure that the
   #  toolboxes can also access the data in an efficient manner. When a
   #  toolbox access this file if will need to save the NDF first to make sure
   #  that the data values are up to date.
   protected method set_step_axis_ {value {setplane 1}} {
      if { $value != $axis_ && $bounds_ != {} } {
         set axis_ $value
         set plane_min_ [lindex $bounds_ [expr (${axis_}-1)*2]]
         set plane_max_ [lindex $bounds_ [expr (${axis_}-1)*2+1]]

         $itk_component(index) configure -from $plane_min_ -to $plane_max_
         if { $setplane } {
            $itk_component(index) configure -value $plane_min_
         }

         $itk_component(spectrum) set_bounds $plane_min_ $plane_max_
         $itk_component(animation) set_bounds $plane_min_ $plane_max_
         $itk_component(collapse) set_bounds $plane_min_ $plane_max_
         $itk_component(chanmap) set_bounds $plane_min_ $plane_max_
         $itk_component(baseline) set_bounds $plane_min_ $plane_max_

         #  Update the display of label and units in index component.
         $itk_component(index) update_coords_type $plane_

         #  Set up object to control units.
         $spec_coords_ configure -axis $axis_ -accessor $cubeaccessor_

         #  Set an initial plane, if requested.
         if { $setplane } {
            set plane_ $plane_min_
            set_display_plane [expr ($plane_max_ + $plane_min_)/2] 1
         } else {
            #  Otherwise update the displayed coordinate for the plane.
            $itk_component(index) configure -value $plane_
         }
      }
   }

   #  Handle a change in the coordinate system. This requires all the local
   #  coordinates to be updated. Same as changing the axis, but without an
   #  actual change.
   protected method coords_changed_ {} {
      set value $axis_
      incr axis_
      set_step_axis_ $value 0
      $itk_component(spectrum) coord_system_changed
   }

   #  Set the plane to display and display it. When regen is true a new
   #  image NDF is displayed, otherwise just the NDF image data is updated.
   public method set_display_plane { newvalue {regen 0} } {

      #  Do nothing, if the plane remains the same.
      if { $newvalue == $plane_ || $ndfname_ == {} } {
         return
      }

      if { $newvalue >= $plane_max_ } {
         set plane_ $plane_max_
      } elseif { $newvalue <= $plane_min_ } {
         set plane_ $plane_min_
      } else {
         set plane_ $newvalue
      }

      #  If regenerating create the dummy NDF. Note this is also needed when
      #  an image has replaced the dummy NDF (collapses, chanmaps etc.).
      #  Check this is by querying the object name, it should change.
      if { ! $regen } {
         set newobject [$itk_option(-rtdimage) object]
         if { $newobject != $fullobject_ } {
            set regen 1
         }
      }

      if { $regen } {

         #  Set name of the image displayed image section.
         set oldname $section_name_
         set section_name_ "GaiaTempCubeSection[incr count_].sdf"

         #  And create the dummy image NDF. Will have axis removed from
         #  the WCS and be the size and type of cube in other axes.
         set imageaccessor [$cubeaccessor_ createimage $section_name_ $axis_]

         #  Map in the data component to initialise it. Use BAD
         #  to avoid NDF bad flag from being set false.
         $imageaccessor map "WRITE/BAD"

         #  Close before it can be displayed by the rtdimage.
         $imageaccessor close

         #  Display this for the first time.
         display $section_name_ 0

         #  Now delete old image slice (waited until released).
         if { $oldname != {} } {
            catch {::file delete $oldname} msg
            if { $msg != {} } {
               puts stderr \
                  "WARNING: Failed to delete old image section: $msg"
            }
         }
      }

      #  Now copy this plane from the cube and update the image.
      #  Correct plane to grid indices.
      if { $axis_ == 1 } {
         set zo [lindex $bounds_ 0]
      } elseif { $axis_ == 2 } {
         set zo [lindex $bounds_ 2]
      } else {
         set zo [lindex $bounds_ 4]
      }
      set zp [expr round($plane_+1-$zo)]

      #  Access the image data for this plane and replace
      #  the displayed copy.
      lassign [$cubeaccessor_ getimage $axis_ $zp 1] adr
      lassign [$cubeaccessor_ getinfo $adr] realadr nel type
      $itk_option(-rtdimage) updateimagedata $realadr

      #  If regenerating or asked bring autocuts etc. into sync.
      if { $regen || $itk_option(-autocut) } {
         $itk_option(-rtdimage) autocut -percent 98
      }

      #  Set the object description of this slice to include the
      #  cube slice.
      set fullobject_ "[slice_display_name_] ($object_)"
      $itk_option(-rtdimage) object $fullobject_
      $rtdctrl_info_ updateValues

      #  Release memory from last time and save pointer.
      if { $last_slice_adr_ != 0 } {
         $cubeaccessor_ release $last_slice_adr_
      }
      set last_slice_adr_ $adr

      #  Make sure position show in slide matches this (note feedback is
      #  avoided as $newvalue != $plane_).
      $itk_component(index) configure -value $plane_
   }

   #  Set the position of a reference line shown in the plot.
   public method set_spec_ref_coord {id coord} {
      $itk_component(spectrum) set_spec_ref_coord $id $coord
   }

   #  Set the position of a reference range shown in the plot.
   public method set_spec_ref_range_coord {id coord1 coord2} {
      $itk_component(spectrum) set_spec_ref_range_coord $id $coord1 $coord2
   }

   #  Handle the change in a spectral reference line, when done within the
   #  plot (user interaction by dragging line).
   protected method ref_line_moved_ {id coord action} {

      #  Convert coord from grid indices to pixel indices.
      set coord [axis_grid2pixel $coord]

      #  Apply value to the right control, but avoiding new actions being
      #  triggered, unless this is release when we want to reposition to the
      #  exact control feedback.
      if { $action == "move" } {
         set oldvalue [$ref_line_controls_($id) cget -show_ref_line]
         $ref_line_controls_($id) configure -show_ref_line 0
      }
      $ref_line_controls_($id) configure -value $coord

      #  And get the associated action to run.
      eval [$ref_line_controls_($id) cget -coord_update_cmd] $coord

      #  If action is released also do the drag_update_cmd.
      if { $action == "released" } {
         eval [$ref_line_controls_($id) cget -drag_update_cmd]
      }
      if { $action == "move" } {
         $ref_line_controls_($id) configure -show_ref_line $oldvalue
      }
   }

   #  Handle the change in a spectral reference range, when done within the
   #  plot (user interaction by dragging line).
   protected method ref_range_moved_ {id coord1 coord2 action} {

      #  Convert coord from grid indices to pixel indices.
      set coord1 [axis_grid2pixel $coord1]
      set coord2 [axis_grid2pixel $coord2]

      #  Set the limits of the control and get it to configure itself.
      if { $id > $baseline_id_ } {
         set id $baseline_id_
      }
      $ref_range_controls_($id) configure -lower_limit $coord1 \
                                          -upper_limit $coord2
      $ref_range_controls_($id) ref_range_moved $id $coord1 $coord2 $action
   }

   #  Update the dummy NDF WCS so that it matches the current spectral
   #  coordinates. This should be done only when necessary (end of animation,
   #  slice slider drag etc.).
   public method update_wcs {} {
      set index [axis_pixel2grid $plane_]
      set frameset [$cubeaccessor_ getimagewcs $axis_ $index]
      if { $frameset != 0 } {
         $itk_option(-rtdimage) astreplace $frameset
      }
   }

   #  Create a description of the slice. Note use short name so should
   #  probably not be used to access any NDF sections.
   protected method slice_display_name_ {} {
      if { $axis_ == 1 } {
         set section "($plane_,,$close_section_"
      } elseif { $axis_ == 2 } {
         set section "(,$plane_,$close_section_"
      } else {
         set section "(,,${plane_}${close_section_}"
      }
      return ${itk_option(-cube)}${section}
   }

   #  Display an image. Note these cannot be temporary in the normal sense,
   #  that messes up the interaction. We deal with temporary images locally.
   public method display {name {istemp 0} } {
      $itk_option(-gaia) configure -check_for_cubes 0
      $itk_option(-gaia) open $name
      if { $istemp } {
         register_temp_file $name
      }
      $itk_option(-gaia) configure -check_for_cubes $check_for_cubes_
   }

   #  Register a temporary file. These will be deleted along with this
   #  object.
   public method register_temp_file {filename} {
      lappend temp_files_ $filename
   }

   #  Get the coordinate of the current plane along the current axis.
   public method get_plane_coord {{formatted 1} {trail 0} } {
      return [get_coord $plane_ $formatted $trail]
   }

   #  Get the coordinate of an index along the current axis.
   public method get_coord {index {formatted 1} {trail 0}} {
      #  Need index as a pixel coordinate.
      set pcoord [expr $index - 0.5]
      if { $axis_ == 1 } {
         set section [list $pcoord 1 1]
      } elseif { $axis_ == 2 } {
         set section [list 1 $pcoord 1]
      } else {
         set section [list 1 1 $pcoord]
      }
      set coord {}
      catch {
         set coord [$cubeaccessor_ getcoord $axis_ $section $formatted $trail]
      }
      return $coord
   }

   #  Add or disable SPLAT bindings used by spectral extraction tool.
   protected method set_splat_bindings_ {} {
      $itk_component(spectrum) configure -use_splat $itk_option(-use_splat)
   }

   #  Convert grid indices to pixel indices along image axes.
   public method image_grid2pixel {x y} {
      if { $axis_ == 1 } {
         set xo [lindex $bounds_ 2]
         set yo [lindex $bounds_ 4]
      } elseif { $axis_ == 2 } {
         set xo [lindex $bounds_ 0]
         set yo [lindex $bounds_ 4]
      } else {
         set xo [lindex $bounds_ 0]
         set yo [lindex $bounds_ 2]
      }

      #  Correct to pixel indices.
      set ix [expr round($x-1+$xo)]
      set iy [expr round($y-1+$yo)]
      return "$ix $iy"
   }

   #  Convert grid index to pixel index along the selected axis.
   public method axis_grid2pixel { value } {
      if { $axis_ == 1 } {
         set zo [lindex $bounds_ 0]
      } elseif { $axis_ == 2 } {
         set zo [lindex $bounds_ 2]
      } else {
         set zo [lindex $bounds_ 4]
      }
      return [expr round($value-1+$zo)]
   }

   #  Convert pixel indices to grid indices along image axes.
   public method image_pixel2grid {x y} {
      if { $axis_ == 1 } {
         set xo [lindex $bounds_ 2]
         set yo [lindex $bounds_ 4]
      } elseif { $axis_ == 2 } {
         set xo [lindex $bounds_ 0]
         set yo [lindex $bounds_ 4]
      } else {
         set xo [lindex $bounds_ 0]
         set yo [lindex $bounds_ 2]
      }

      #  Correct to grid indices.
      set ix [expr round($x+1-$xo)]
      set iy [expr round($y+1-$yo)]
      return "$ix $iy"
   }

   #  Convert pixel index to grid index along the selected axis.
   public method axis_pixel2grid { value } {
      if { $axis_ == 1 } {
         set zo [lindex $bounds_ 0]
      } elseif { $axis_ == 2 } {
         set zo [lindex $bounds_ 2]
      } else {
         set zo [lindex $bounds_ 4]
      }
      return [expr round($value+1-$zo)]
   }

   #  ==========================================
   #  Utility methods for various helper classes
   #  ==========================================

   #  Make a reference range in the spectral plot, if shown. Use the given
   #  identifier.
   public method make_ref_range {id} {
      $itk_component(spectrum) make_ref_range $id
   }

   #  Remove a reference range from the spectral plot, if shown. Use the given
   #  identifier.
   public method remove_ref_range {id} {
      $itk_component(spectrum) remove_ref_range $id
   }

   #  Set the colour of the identified reference range.
   public method set_ref_range_colour {id colour} {
      $itk_component(spectrum) set_ref_range_colour $id $colour
   }

   #  Get the name of the cube (in NDF format).
   public method get_ndfname {} {
      return $ndfname_
   }

   #  Get the current axis.
   public method get_axis {} {
      return $axis_
   }

   #  Allow support classes access to the cubeaccessor. This access
   #  should be considered readonly.
   public method get_cubeaccessor {} {
      return $cubeaccessor_
   }

   #  Get the current axis.
   public method get_close_section {} {
      return $close_section_
   }

   #  Set the spectral extraction range. Tools that display collapsed images
   #  will probably want to do this.
   public method set_extraction_range {lower upper} {
      $itk_component(spectrum) ref_range_moved 1 $lower $upper none
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the input text file.
   itk_option define -cube cube Cube {} {
      set_chosen_cube_
   }

   #  Name of the Gaia instance we're controlling.
   itk_option define -gaia gaia Gaia {} {
      set check_for_cubes_ [$itk_option(-gaia) cget -check_for_cubes]
      set rtdctrl [$itk_option(-gaia) get_image]
      set rtdctrl_info_ [$rtdctrl component info]
   }

   #  The canvas.
   itk_option define -canvas canvas Canvas {}

   #  The rtdimage instance.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Filters for selecting files.
   itk_option define -filter_types filter_types Filter_types {}

   #  Whether to autoscale the cuts as every image slice is displayed.
   itk_option define -autocut autocut AutoCut 0

   #  Whether to use file mapping (quick startup), or direct io (fast
   #  spectral display).
   itk_option define -usemmap usemmap UseMmap 1

   #  Whether to enable or disabled the SPLAT mouse bindings.
   itk_option define -use_splat use_splat Use_Splat 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Data access object for the cube.
   protected variable cubeaccessor_ {}

   #  The bounds of the cube, 3 pairs of upper and lower values.
   protected variable bounds_ {}

   #  The name of the dataset, as an NDF specification (handle HDU and slices).
   protected variable ndfname_ {}

   #  The current plane along the current axis.
   protected variable plane_ 1

   #  Maximum and minimum possible value for plane.
   protected variable plane_max_ 0
   protected variable plane_min_ 0

   #  The current axis.
   protected variable axis_ 1

   #  Check for cubes setting of GAIA.
   protected variable check_for_cubes_ 1

   #  The name for the dummy NDF, with updatable image section.
   protected variable section_name_ ""

   #  Memory used for last slice. Free this when not needed.
   protected variable last_slice_adr_ 0

   #  The terminator characters for closing a section. May specify
   #  a final redundant axis.
   protected variable close_section_ ")"

   #  The object value set in the cube (used in slice object).
   protected variable object_ {}

   #  The object value set in the main display. Used to check we still
   #  have control of the dummy slice.
   protected variable fullobject_ {}

   #  GaiaImagePanel object used in the main window. Forced to update to
   #  reveal the slice information.
   protected variable rtdctrl_info_ {}

   #  Object for controlling the spectral coordinates.
   protected variable spec_coords_ {}

   #  Array of index-based controls for the reference lines. These are indexed
   #  by their ref_id.
   protected variable ref_line_controls_

   #  Array of index-based controls for the reference ranges. These are indexed
   #  by their ref_id.
   protected variable ref_range_controls_

   #  A list of temporary files, these will be deleted when the object
   #  is destroyed.
   protected variable temp_files_ ""

   #  The ref_id value of the baseline controls. Must be last.
   protected variable baseline_id_ ""

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The temporary image count.
   common count_ 0

#  End of class definition.
}

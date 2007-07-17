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
#     Copyright (C) 2006-2007 Particle Physics & Astronomy Research Council.
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

      set lwidth 18
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

      #  Add the View menu.
      set View [add_menubutton "View" left]
      configure_menubutton View -underline 0

      #  View the cube FITS headers.
      $View add command -label "Fits header..." \
         -command [code $this show_fitsheaders_] \
         -accelerator {Control-f}
      bind $w_ <Control-f> [code $this show_fitsheaders_]

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

      #  Choice of cube component.
      set submenu [menu $Options.component]
      $Options add cascade -label "Data component" -menu $submenu
      foreach type {DATA VARIANCE QUALITY} {
         $submenu add radiobutton \
            -variable [scope component_] \
            -value $type \
            -label $type \
            -command [code $this component_changed_]
      }

      #  Add the coordinate selection menu.
      set SpectralCoords [add_menubutton "Coords" left]
      configure_menubutton "Coords" -underline 0
      set spec_coords_ [GaiaSpecCoords \#auto \
                           -change_cmd [code $this coords_changed_]]
      $spec_coords_ add_menu $SpectralCoords

      #  Add the "Go" menu to switch back and forth between cubes that have
      #  been loaded.
      set GoMenu [add_menubutton "Go" \
                  "Go: menu with shortcuts to view cubes previously viewed"]
      configure_menubutton "Go" -underline 0
      $GoMenu config -postcommand [code $this update_history_menu_ $GoMenu]

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
            -command [code $this configure -cube] \
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
            -show_increment 1 \
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
         iwidgets::tabnotebook $w_.tab -equaltabs 0 \
            -angle 0 -tabpos n -width 420 -height 525
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

      $itk_component(tabnotebook) add -label Rebin
      set rebinTab [$itk_component(tabnotebook) childsite 4]

      $itk_component(tabnotebook) add -label Filter
      set filterTab [$itk_component(tabnotebook) childsite 5]

      $itk_component(tabnotebook) add -label Baseline
      set baselineTab [$itk_component(tabnotebook) childsite 6]

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
            -spec_coords [code $spec_coords_] \
            -component $component_ \
            -transient_spectralplot $itk_option(-transient_spectralplot)

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
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
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
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
      }
      set ref_range_controls_($ref_ids) $itk_component(chanmap)
      pack $itk_component(chanmap) -side top -fill both -ipadx 1m -ipady 2m

      #  Rebin section.

      itk_component add rebinruler {
         LabelRule $rebinTab.rebinruler -text "Rebin cube controls:"
      }
      pack $itk_component(rebinruler) -side top -fill x

      itk_component add rebin {
         GaiaCubeRebin $rebinTab.rebin \
            -gaiacube [code $this] \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
      }
      pack $itk_component(rebin) -side top -fill both -ipadx 1m -ipady 2m

      #  Filter section.

      itk_component add filterruler {
         LabelRule $filterTab.filterruler -text "Filter cube controls:"
      }
      pack $itk_component(filterruler) -side top -fill x

      itk_component add filter {
         GaiaCubeFilter $filterTab.filter \
            -gaiacube [code $this] \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
      }
      pack $itk_component(filter) -side top -fill both -ipadx 1m -ipady 2m

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
            -valuewidth $vwidth \
            -spec_coords [code $spec_coords_]
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

      #  Release the cube.
      catch {release}

      #  Stop interactions.
      catch {halt}

      #  Delete the NDF image slice.
      if { $section_name_ != {} } {
         catch {::file delete $section_name_} msg
      }

      #  Delete any registered temporary files.
      foreach filename $temp_files_ {
         if { [file exists $filename] } {
            catch {::file delete $filename}
         }
      }

   }

   #  Methods:
   #  --------

   #  Close window.
   public method close {} {
      wm withdraw $w_
      halt
   }

   #  Halt all interactive operations.
   public method halt {} {

      #  Close spectrum plot.
      $itk_component(spectrum) close

      #  Halt any animations.
      $itk_component(animation) stop

      #  If displaying a channel map remove the markers.
      $itk_component(chanmap) close

      #  Destroy FITS headers window.
      if { $fitsheaders_ != {} } {
         catch {
            destroy $fitsheaders_
            set fitsheaders_ {}
         }
      }
   }

   #  Undo some of the effects of close. Do not reopen cube, that would
   #  be expensive and reset too much. Do that using release close/halt.
   public method open {} {
      $itk_component(spectrum) open
   }

   #  Release the cube, making ready for new cube or object destruction.
   #  Don't use this unless that's what you want. Returns 1 if a cube
   #  was open.
   public method release {} {
      if { $cubeaccessor_ != {} } {
         set isopen [$cubeaccessor_ close]
         set cubeaccessor_ {}
         set itk_option(-cube) {}
         set ndfname_ {}
         set last_cube_ {}
         return $isopen
      }
      return 0
   }

   #  Open a new cube, restoring the current extraction limits, if possible.
   public method open_keeplimits {filename} {
      set itk_option(-cube) $filename
      set_chosen_cube_ 1
   }

   #  Open the currently chosen file as a cube. If requested attempt to
   #  preserve the extraction limits through the change, even if the cube
   #  dimensions change.
   protected method set_chosen_cube_ {{keeplimits 0}} {

      set namer [GaiaImageName \#auto -imagename $itk_option(-cube)]
      if { ! [$namer exists] } {
         if { $itk_option(-cube) != {} } {
            error_dialog "No such file: $itk_option(-cube)" $w_
            record_last_cube_
         }
         return
      } else {

         #  Add last_cube_ to the back_list_, including temporary ones.
         record_last_cube_

         #  Always start with the DATA component (essential).
         set component_ "DATA"
         set last_component_ "DATA"

         if { $cubeaccessor_ == {} } {
            set cubeaccessor_ [uplevel \#0 GaiaNDAccess \#auto]
         } elseif { $keeplimits } {

            #  Existing cube accessor and keeping limits regardless, so we
            #  need to save some properties before letting this go.
            save_limits_
         }

         $namer absolute
         set ndfname_ [$namer ndfname 0]
         set type_ [$namer type]

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
            record_last_cube_
            return
         }

         #  Map in all data, this makes it immediately available within
         #  application
         load_cube_ 1

         #  Retain the TITLE value for creating new object values.
         set object_ [$cubeaccessor_ getc "TITLE"]

         #  Set axis and display the plane for first time. Try to pick
         #  out a spectral axis.
         if { [$cubeaccessor_ isaxisframetype 3 "specframe"] } {
            set axis_ 2
            set axis 3
         } elseif { [$cubeaccessor_ isaxisframetype 2 "specframe"] } {
            set axis_ 1
            set axis 2
         } elseif { [$cubeaccessor_ isaxisframetype 1 "specframe"] } {
            set axis_ 2
            set axis 1
         } else {
            set axis_ 2
            set axis 3
         }
         $itk_component(axis) configure -value $axis
         set_step_axis_ $axis

         #  Set up object to control units.
         $spec_coords_ configure -axis $axis -accessor $cubeaccessor_

         #  If the spectral plot is open, then close it. It will be
         #  re-created on next image click.
         $itk_component(spectrum) close_plot

         #  If keeping the limits, regardless, then attempt the restore.
         if { $keeplimits } {
            restore_limits_
         }

         #  Add cube to history (and previous cube to back list).
         add_history $itk_option(-cube)

         #  Update FITS headers, if viewed.
         maybe_update_fitsheaders_
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
            set adr [$cubeaccessor_ map "READ" "DATA"]
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
   #  toolbox access this file it will need to save the NDF first to make sure
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

         foreach type "spectrum animation collapse chanmap baseline" {
            $itk_component($type) set_bounds $plane_min_ $plane_max_
         }

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
      $itk_component(spectrum) coord_system_changed

      set value $axis_
      incr axis_
      set_step_axis_ $value 0
   }

   #  Set the plane to display and display it. When regen is true a new
   #  image NDF is displayed, otherwise just the NDF image data is updated.
   public method set_display_plane { newvalue {regen 0} } {

      #  Do nothing, if the plane remains the same.
      if { ( $newvalue == $plane_ && ! $regen ) || $ndfname_ == {} } {
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

         #  Make sure everything is up-to-date (new data has been accepted by
         #  the RtdImage) so that the data replacement happens on the file
         #  we've just loaded (can get out of sync when unexpected errors
         #  autoloading Tcl scripts occur, which defer the newImage acceptance
         #  callbacks).
         update

         #  Cube may have been waiting for clear.
         if { $cubeaccessor_ == {} } {
            return
         }

         #  Now delete old image slice (waited until released).
         if { $oldname != {} } {
            catch {::file delete $oldname} msg
            if { $msg != {} } {
               puts stderr \
                  "WARNING: Failed to delete old image section: $msg"
            }
         }

         #  Enable any bindings etc. in the spectral extraction tool.
         $itk_component(spectrum) open

         #  Remove any channel map markers.
         $itk_component(chanmap) close
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
      set adr [$cubeaccessor_ getimage $axis_ $zp 1 $component_]
      lassign [$cubeaccessor_ getinfo $adr] realadr nel type
      $itk_option(-rtdimage) replaceimagedata $realadr

      #  If regenerating or asked bring autocuts etc. into sync.
      if { $regen || $itk_option(-autocut) } {
         $itk_option(-rtdimage) autocut -percent 98
      }

      #  Set the object description of this slice to include the
      #  cube slice. Get it back for comparison (can be truncated)"
      $itk_option(-rtdimage) object "[slice_display_name_] ($object_)"
      set fullobject_ [$itk_option(-rtdimage) object]
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
      set cid $id
      if { $id > $baseline_id_ } {
         set cid $baseline_id_
      }
      $ref_range_controls_($cid) configure \
         -lower_limit $coord1 -upper_limit $coord2
      $ref_range_controls_($cid) ref_range_moved $id $coord1 $coord2 $action
   }

   #  Update the dummy NDF WCS so that it matches the current spectral
   #  coordinates. This should be done only when necessary (end of animation,
   #  slice slider drag etc.).
   public method update_wcs {} {
      if { $cubeaccessor_ != {} } {
         set index [axis_pixel2grid $plane_]
         set frameset [$cubeaccessor_ getimagewcs $axis_ $index]
         if { $frameset != 0 } {
            $itk_option(-rtdimage) astreplace $frameset
         }
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

   #  Should be called when the data component has changed.
   protected method component_changed_ {} {

      if { $cubeaccessor_ != {} } {
         if { [$cubeaccessor_ exists $component_] } {
            #  Only the DATA component is mapped by default, so make sure
            #  we access the others too.
            if { $component_ != "DATA" } {
               set adr [$cubeaccessor_ map "READ" $component_]
            }

            #  Cause an update of the image.
            set_display_plane $plane_ 1
         } else {
            #  No such component....
            info_dialog "No $component_ component in cube" $w_
            if { $component_ != $last_component_ } {
               set component_ $last_component_
            }
            return
         }
      }

      #  Get extracted spectra to display this component.
      if { [info exists itk_component(spectrum)] } {
         $itk_component(spectrum) configure -component $component_
      }

      #  Fallback when component doesn't exist.
      set last_component_ $component_
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

   #  Get the type of the cube.
   public method get_type {} {
      return $type_
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

   #  ============================================================
   #  Go menu control. Just like that of Skycat, except filters out
   #  cubes and allows the history to be updated without opening as
   #  an image.
   #  =============================================================

   #  Add a cube to the history catalog under the given filename.
   #  Re-implementation of SkySearch::add_history that avoids using the
   #  rtdimage to obtain the cube properties.
   public method add_history {filename} {

      #  Ignore non-existant cubes.
      if { $filename == "" || ! [file exists $filename] } {
         return
      }

      #  Access history catalogue (need to do this sometime, so do it now).
      set catalog $history_catalog_

      #  Check if the directory for the catalog exists
      set dir [file dirname $catalog]
      if { ! [file isdirectory $dir] } {
         if { [catch {exec mkdir $dir} msg] } {
            warning_dialog $msg
            return
         }
      }

      #  Make sure at least an empty catalog exists.
      if { ! [file exists $catalog] || [file size $catalog] == 0 } {

         # If it doesn't exist yet, create an empty catalog file
         if { [catch {set fd [::open $catalog w]} msg] } {
            warning_dialog "can't create image history catalog: $msg"
            return
         }
         puts $fd "Skycat History Catalog v1.0"
         puts $fd ""
         puts $fd "ra_col: -1"
         puts $fd "dec_col: -1"
         puts $fd "x_col: -1"
         puts $fd "y_col: -1"
         puts $fd "show_cols: file ra dec object NAXIS NAXIS1 NAXIS2 NAXIS3"
         puts $fd "sort_cols: timestamp"
         puts $fd "sort_order: decreasing"
         puts $fd ""
         puts $fd [join $history_cols_ "\t"]
         puts $fd "----"
         ::close $fd

         #  Get the catalog into the list of known catalogs.
         $astrocat_ open $catalog
      }

      #  Don't record Temp cubes they will be deleted.
      if { ! [string match {*Temp*} $filename] } {

         #  Add an entry for the given image and filename
         set id [file tail $filename]

         #  Image centre RA and Dec not easily available, so skip (need image
         #  loaded).
         set ra "00:00:00"
         set dec "00:00:00"

         #  Record cube dimensions.
         set object [$cubeaccessor_ fitsread OBJECT]
         set naxis 3
         set dims [$cubeaccessor_ getdims 0]
         set naxis1 [lindex $dims 0]
         set naxis2 [lindex $dims 0]
         set naxis3 [lindex $dims 0]

         #  Also make these up.
         set lowcut 0
         set highcut 1
         set colormap "real.lasc"
         set itt "ramp.iasc"
         set colorscale "linear"
         set zoom "1"

         set timestamp [clock seconds]

         #  Get full path name of file for preview URL
         if { "[string index $filename 0]" == "/" } {
            set fullpath $filename
         } else {
            set fullpath [pwd]/$filename
         }
         set preview file:$fullpath

         set data [list [list $id $ra $dec $object \
                            $naxis $naxis1 $naxis2 $naxis3 \
                            $lowcut $highcut $colormap $itt $colorscale $zoom \
                            $timestamp $preview]]

         $astrocat_ open $catalog
         $astrocat_ save $catalog 1 $data ""

         #  Update history catalog window, if it is showing
         set w [cat::AstroCat::get_instance [file tail $catalog]]
         if { "$w" != "" && [winfo viewable $w] } {
            $w search
         }
      }
   }

   #  SkyCatCtrl like update_history_menu, but filter to only show
   #  cubes.
   protected method update_history_menu_ {menu} {

      #  Clear existing items.
      $menu delete 0 end

      add_menuitem $menu command "Back" \
         {Go back again to the previous cube} \
         -command [code $this previous_cube] \
         -state disabled

      if { [info exists back_list_(cube)] && [llength $back_list_(cube)] } {
         $menu entryconfig Back -state normal
      }

      add_menuitem $menu command "Forward" \
         {Go forward again to the next image} \
         -command [code $this forward_cube] \
         -state disabled

      if { [info exists forward_list_(cube)] &&
            [llength $forward_list_(cube)] } {
         $menu entryconfig Forward -state normal
      }

      $menu add separator

      add_history_menu_items $menu 20
   }

   #  SkySearch::add_history_menu_items, implemented to filter only
   #  the first n cubes from the history list.
   protected method add_history_menu_items {menu n} {

      set catalog $history_catalog_
      if { [catch {$astrocat_ open $catalog} ] } {
         #  No catalog yet
         return
      }
      set list [$astrocat_ query \
                   -nrows $n -sort timestamp -sortorder decreasing]

      foreach row $list {
         eval lassign {$row} $history_cols_
         if { $NAXIS3 != {} } {
            set filename [string range $PREVIEW 5 end]
            $menu add command \
               -label $file \
               -command [code $this configure -cube $filename]
         }
      }
   }

   #  Go back to the previous cube.
   public method previous_cube {} {

      while { [set n [llength $back_list_(cube)]] } {

         set filename [lindex $back_list_(cube) end]
         set system [lindex $back_list_(system) end]
         set units [lindex $back_list_(units) end]

         if { "$filename" != "$itk_option(-cube)" &&
              [file exists $filename] } {

            #  Current file becomes forward.
            lappend forward_list_(cube) $itk_option(-cube)
            lassign [$spec_coords_ get_system] oldsystem oldunits
            if { $oldsystem == "default" } {
               set oldsystem {}
               set oldunits {}
            }
            lappend forward_list_(system) $oldsystem
            lappend forward_list_(units) $oldunits

            #  Remove cube we're about to display from back list. This
            #  will be added again when a new cube is loaded.
            set back_list_(cube) [lrange $back_list_(cube) 0 [expr $n-2]]
            set back_list_(system) [lrange $back_list_(system) 0 [expr $n-2]]
            set back_list_(units) [lrange $back_list_(units) 0 [expr $n-2]]

            #  Ok, now switch to previous file, making sure the current
            #  file isn't recorded (goes onto the forward list).
            set last_cube_ {}

            #  Open cube, attempting to keep any world coordinate limits.
            open_keeplimits $filename

            #  And its system/units.
            if { $system != {} } {
               $spec_coords_ set_system $system $units 0
            }
            break
         }

         #  Skip non-existent files.
         set back_list_(cube) [lrange $back_list_(cube) 0 [expr $n-2]]
         set back_list_(system) [lrange $back_list_(system) 0 [expr $n-2]]
         set back_list_(units) [lrange $back_list_(units) 0 [expr $n-2]]
      }
   }

   #  Go forward again to the next cube.
   public method forward_cube {} {

      while { [set n [llength $forward_list_(cube)]] } {
         set filename [lindex $forward_list_(cube) end]
         set system [lindex $forward_list_(system) end]
         set units [lindex $forward_list_(units) end]
         if {"$filename" != "$itk_option(-cube)" && [file exists $filename]} {

            #  Remove this from lists.
            set forward_list_(cube) \
               [lrange $forward_list_(cube) 0 [expr $n-2]]
            set forward_list_(system) \
               [lrange $forward_list_(system) 0 [expr $n-2]]
            set forward_list_(units) \
               [lrange $forward_list_(units) 0 [expr $n-2]]

            #  Load and display, attempting to keep world coordinate limits.
            open_keeplimits $filename

            #  Set related system/units.
            if { $system != {} } {
               $spec_coords_ set_system $system $units 0
            }
            break
         }

         #  Skip non-existent files.
         set forward_list_(cube) \
            [lrange $forward_list_(cube) 0 [expr $n-2]]
         set forward_list_(system) \
            [lrange $forward_list_(system) 0 [expr $n-2]]
         set forward_list_(units) \
            [lrange $forward_list_(units) 0 [expr $n-2]]
      }
   }

   #  Add the "last cube" to the last_cube_ list.
   protected method record_last_cube_ {} {

      #  Don't add if present as last cube or no cube loaded.
      if { $last_cube_ != {} } {
         if { [info exists back_list_(cube)] } {
            set current_last [lindex $back_list_(cube) end]
         } else {
            set current_last {}
         }

         if { $current_last != $last_cube_ } {
            lappend back_list_(cube) $last_cube_

            lassign [$spec_coords_ get_system] system units
            if { $system == "default" } {
               set system {}
               set units {}
            }
            lappend back_list_(system) $system
            lappend back_list_(units) $units
         }
      }

      set last_cube_ $itk_option(-cube)
   }


   #  =================
   #  Methods involved in "saving" the current extraction, animation
   #  etc. limits.
   #  =================

   protected method save_limits_ {} {
      catch {unset limits_}

      if { $cubeaccessor_ != {} } {

         #  Save a copy of the current frameset, that defines the world
         #  coordinates.
         set wcs [$cubeaccessor_ getwcs]
         set limits_(frame) [gaiautils::getframe $wcs "current"]

         #  Axis all the limits apply too.
         set limits_(axis) $axis_

         #  Save all limits in world coordinates. Only done when these
         #  are different to bounds.
         foreach type "spectrum animation collapse chanmap" {
            set bounds [$itk_component($type) get_set_limits]
            if { $bounds != {} } {
               set limits_($type,0) [get_coords [lindex $bounds 0]]
               set limits_($type,1) [get_coords [lindex $bounds 1]]
            }
         }

         #  Baseline has several bounds. Need to visit them all.
         set limits_(baseline,nr) [$itk_component(baseline) cget -nranges]
         for {set i 0} {$i < $limits_(baseline,nr)} {incr i} {
            set bounds [$itk_component(baseline) get_set_limits $i]
            if { $bounds != {} } {
               set limits_(baseline,$i,0) [get_coords [lindex $bounds 0]]
               set limits_(baseline,$i,1) [get_coords [lindex $bounds 1]]
            }
         }
      }
   }

   protected method restore_limits_ {} {
      if { [info exists limits_] && $cubeaccessor_ != {} } {

         #  If not same axis, then don't bother. The other axis limits are
         #  faked.
         if { $limits_(axis) == $axis_ } {

            #  Connect the old frame defining the old coordinate system to the
            #  current WCS.
            set wcs [$cubeaccessor_ getwcs]

            #  Use an inverted WCS so we connect to BASE, not just CURRENT.
            gaiautils::astset $wcs "Invert=1"
            set convwcs [gaiautils::astconvert $limits_(frame) $wcs ""]
            gaiautils::astset $wcs "Invert=0"

            if { $convwcs != 0 } {

               set axis [expr $axis_-1]

               foreach type "spectrum animation collapse chanmap" {
                  if { [info exists limits_($type,0)] } {

                     set li [gaiautils::asttrann $convwcs $limits_($type,0)]
                     set ui [gaiautils::asttrann $convwcs $limits_($type,1)]

                     #  Convert to integers.
                     set li [expr int([lindex $li $axis])]
                     set ui [expr int([lindex $ui $axis])]

                     #  And set.
                     $itk_component($type) apply_limits $li $ui
                  }
               }

               #  Restore baselines.
               for {set i 0} {$i < $limits_(baseline,nr)} {incr i} {
                  if { [info exists limits_(baseline,$i,0)] } {
                     set li \
                        [gaiautils::asttrann $convwcs $limits_(baseline,$i,0)]
                     set ui \
                        [gaiautils::asttrann $convwcs $limits_(baseline,$i,1)]

                     #  Convert to integers.
                     set li [expr int([lindex $li $axis])]
                     set ui [expr int([lindex $ui $axis])]

                     #  And set.
                     $itk_component(baseline) apply_limits $i $li $ui
                  }
               }
            }
         }
      }

   }

   #  Get the coordinates of an index along the current axis. Returns
   #  a list of 3 or 4 values, depending on cube dimensions.
   public method get_coords {index} {

      #  Need index as a pixel coordinate.
      set pcoord [expr $index - 0.5]
      if { $close_section_ != ")" } {
         if { $axis_ == 1 } {
            set section [list $pcoord 1 1 1]
         } elseif { $axis_ == 2 } {
            set section [list 1 $pcoord 1 1]
         } else {
            set section [list 1 1 $pcoord 1]
         }
      } else {
         if { $axis_ == 1 } {
            set section [list $pcoord 1 1]
         } elseif { $axis_ == 2 } {
            set section [list 1 $pcoord 1]
         } else {
            set section [list 1 1 $pcoord]
         }
      }
      set coords {}
      catch {
         set wcs [$cubeaccessor_ getwcs]
         set coords [gaiautils::asttrann $wcs $section]
      }
      return $coords
   }
   
   #  =======
   #  FITS headers
   #  =======

   #  Create and display the FITS headers dialog window.
   protected method show_fitsheaders_ {} {
      if { $fitsheaders_ == {} } {
         set fitsheaders_ $w_.fitsheaders_
      }
      utilReUseWidget GaiaFITSHeader $fitsheaders_ -accessor $cubeaccessor_
      $fitsheaders_ activate
   }

   #  If displaying the FITS headers update to show headers from current cube.
   protected method maybe_update_fitsheaders_ {} {
      if { $fitsheaders_ != {} } {
         if { [winfo viewable $fitsheaders_] } {
            $fitsheaders_ activate
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the input cube. NDF or FITS specification.
   itk_option define -cube cube Cube {} {
      set_chosen_cube_
   }

   #  Name of the Gaia instance we're controlling.
   itk_option define -gaia gaia Gaia {} {
      set check_for_cubes_ [$itk_option(-gaia) cget -check_for_cubes]
      set rtdctrl_ [$itk_option(-gaia) get_image]
      set rtdctrl_info_ [$rtdctrl_ component info]
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

   #  Whether spectral plot window should be a transient, or not.
   itk_option define -transient_spectralplot transient_spectralplot \
      Transient_Spectralplot 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Data access object for the cube.
   protected variable cubeaccessor_ {}

   #  The component to display/extract. Usually DATA but could be VARIANCE
   #  or QUALITY. Keep last component value as a fallback.
   protected variable component_ "DATA"
   protected variable last_component_ "DATA"

   #  Name of the last cube opened.
   protected variable last_cube_ {}

   #  The bounds of the cube, 3 pairs of upper and lower values.
   protected variable bounds_ {}

   #  The name of the dataset, as an NDF specification (handle HDU and slices).
   protected variable ndfname_ {}

   #  The filetype of the dataset, will be ".sdf" for NDFs.
   protected variable type_ {}

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

   #  GaiaImageCtrl object used in the main window.
   protected variable rtdctrl_ {}

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

   #  Arrays used for the Go=>Back/Forward menu items.
   protected variable back_list_
   protected variable forward_list_

   #  Data associated with the state of all the limits. Used to restore
   #  between cubes with different sizes.
   protected variable limits_

   #  Name of GaiaFITSHeader instance, only set when active.
   protected variable fitsheaders_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The temporary image count.
   protected common count_ 0

   #  Name of the history catalogue, shared with Skycat and images.
   protected common history_catalog_ $::env(HOME)/.skycat/history

   #  C++ astrocat object shared with AstroCat?
   protected common astrocat_ [astrocat ::cat::.astrocat]

   #  List of columns in the history catalog.
   protected common history_cols_ \
      [list file ra dec object NAXIS NAXIS1 NAXIS2 NAXIS3 \
          lowcut highcut colormap itt colorscale zoom timestamp PREVIEW]


#  End of class definition.
}

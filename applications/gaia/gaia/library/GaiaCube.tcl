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

      set lwidth 20

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
            -valuewidth 20 \
            -coord_update_cmd [code $this set_display_plane_] \
            -drag_update_cmd [code $this update_wcs_]
      }
      set index_control_(1) $itk_component(index)
      pack $itk_component(index) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(index) \
         {Index of the image plane to display (along current axis)}

      #  Add tab window for choosing either the animation or collapse
      #  controls.
      itk_component add tabnotebook {
         iwidgets::tabnotebook $w_.tab \
            -angle 0 -tabpos n -width 350 -height 350
      }
      pack $itk_component(tabnotebook) -fill both -expand 1

      $itk_component(tabnotebook) add -label Animation
      set animationTab [$itk_component(tabnotebook) childsite 0]

      $itk_component(tabnotebook) add -label Collapse
      set collapseTab [$itk_component(tabnotebook) childsite 1]

      #  Animation section.
      #
      #  Choose upper and lower limits, and type of animation.
      itk_component add aruler {
         LabelRule $animationTab.aruler -text "Animation controls:"
      }
      pack $itk_component(aruler) -side top -fill x

      #  Whether to show the reference lines on the plot.
      itk_component add showanlines {
         StarLabelCheck $animationTab.showlines \
            -text "Show limits on plot:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope show_animation_lines_] \
            -command [code $this toggle_show_animation_lines_]
      }
      pack $itk_component(showanlines) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(showanlines) \
         {Show extent of animation on plot with reference lines}

      itk_component add lower {
         GaiaSpectralPlotLine $animationTab.lower \
            -gaiacube [code $this] \
            -ref_id 2 \
            -text {Lower index:} \
            -value $plane_ \
            -show_type 0 \
            -show_ref_line $show_animation_lines_ \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -coord_update_cmd [code $this set_animate_lower_bound_]
      }
      set index_control_(2) $itk_component(lower)
      pack $itk_component(lower) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(lower) \
         {Lower index used during animation}

      itk_component add upper {
         GaiaSpectralPlotLine $animationTab.upper \
            -gaiacube [code $this] \
            -ref_id 3 \
            -text {Upper index:} \
            -value $plane_ \
            -show_type 0 \
            -show_ref_line $show_animation_lines_ \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -coord_update_cmd [code $this set_animate_upper_bound_]
      }
      set index_control_(3) $itk_component(upper)
      pack $itk_component(upper) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(upper) \
         {Upper index used during animation}

      #  Delay used in animation.
      itk_component add delay {
         LabelEntryScale $animationTab.delay \
            -text {Delay (milli):} \
            -value $itk_option(-delay) \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -from 10 \
            -to 1000 \
            -increment 10 \
            -resolution 10 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_delay_]
      }
      pack $itk_component(delay) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(delay) \
         {Delay used during animation in milliseconds}

      #  Step between planes.
      itk_component add step {
         LabelEntryScale $animationTab.step \
            -text {Step:} \
            -value $itk_option(-step) \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -from 1 \
            -to 100 \
            -increment 2 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_step_]
      }
      pack $itk_component(step) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(step) \
         {Step between frames of animation}

      #  Looping behaviour.
      itk_component add loopframe {
         frame $animationTab.frame
      }
      itk_component add looplabel {
         label $itk_component(loopframe).label \
            -text "Looping:" \
            -width 21 \
            -anchor w
      }
      pack $itk_component(looplabel) -side left -fill none

      itk_component add loopoff {
         radiobutton $itk_component(loopframe).noloop \
            -text "Off" \
            -variable [scope loop_] \
            -value "off"
      }
      pack $itk_component(loopoff) -side left -fill none
      add_short_help $itk_component(loopoff) \
         {Looping of animation off}

      itk_component add loopon {
         radiobutton $itk_component(loopframe).loopon \
            -text "On" \
            -variable [scope loop_] \
            -value "on"
      }
      pack $itk_component(loopon) -side left -fill none
      add_short_help $itk_component(loopon) \
         {Looping on, restarts from beginning when at end}

      itk_component add looprocknroll {
         radiobutton $itk_component(loopframe).rocknroll \
            -text "Rock 'n Roll" \
            -variable [scope loop_] \
            -value "rocknroll"
      }
      pack $itk_component(looprocknroll) -side left -fill none
      add_short_help $itk_component(looprocknroll) \
         {Looping on, goes into reverse when at end}

      pack $itk_component(loopframe) -side top -fill x -ipadx 1m -ipady 2m


      #  Animation stop and start.
      itk_component add animation {
         frame $animationTab.animation
      }
      pack $itk_component(animation) -side top -fill x -ipadx 1m -ipady 2m

      itk_component add stop {
         button $itk_component(animation).stop -text Stop \
            -command [code $this stop_]
      }
      pack $itk_component(stop) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(stop) {Stop animation}

      itk_component add start {
         button $itk_component(animation).start -text Start \
            -command [code $this start_]
      }
      pack $itk_component(start) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(start) {Start animation}

      #  Collapse section.
      #
      #  Use limits to create a collapsed image using KAPPA COLLAPSE.
      itk_component add cruler {
         LabelRule $collapseTab.cruler -text "Collapse controls:"
      }
      pack $itk_component(cruler) -side top -fill x

      #  Whether to show the reference lines on the plot.
      itk_component add showcollines {
         StarLabelCheck $collapseTab.showlines \
            -text "Show limits on plot:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $lwidth \
            -variable [scope show_collapse_lines_] \
            -command [code $this toggle_show_collapse_lines_]
      }
      pack $itk_component(showcollines) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(showcollines) \
         {Show extent of collapse on plot with reference lines}

      itk_component add collower {
         GaiaSpectralPlotLine $collapseTab.collower \
            -gaiacube [code $this] \
            -ref_id 4 \
            -text {Lower index:} \
            -value 1 \
            -show_type 0 \
            -show_ref_line 0 \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -coord_update_cmd [code $this set_collapse_lower_bound_]
      }
      set index_control_(4) $itk_component(collower)
      pack $itk_component(collower) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(collower) \
         {Lower index used for creating collapsed image}

      itk_component add colupper {
         GaiaSpectralPlotLine $collapseTab.colupper \
            -gaiacube [code $this] \
            -ref_id 5 \
            -text {Upper index:} \
            -value 1 \
            -show_type 0 \
            -show_ref_line 0 \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -coord_update_cmd [code $this set_collapse_upper_bound_]
      }
      set index_control_(5) $itk_component(colupper)
      pack $itk_component(colupper) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(colupper) \
         {Upper index used for creating collapsed image}

      #  Method used for collapse.
      itk_component add combination {
         LabelMenu $collapseTab.cattype \
            -labelwidth $lwidth \
            -text "Combination method:" \
            -variable [scope combination_type_]
      }
      pack $itk_component(combination) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(combination) \
         {Method to use when combining data, use median with care}

      foreach {sname lname} $estimators_ {
            $itk_component(combination) add \
               -label $lname \
               -value $sname \
               -command [code $this set_combination_type_ $sname]
      }

      itk_component add collapse {
         button $collapseTab.collapse -text Collapse \
            -command [code $this collapse_]
      }
      pack $itk_component(collapse) -side top -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(collapse) \
         {Display the combined image collapsed along the range}

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

      #  Stop animation.
      stop_

      #  Release collapser task.
      if { $collapser_ != {} } {
         catch {$collapser_ delete_sometime}
         set collapser_ {}
      }

      #  Close spectrum plot.
      if { $spectrum_ != {} && [winfo exists $spectrum_] } {
         $spectrum_ close
      }
      if { $position_mark_ != {} } {
         $itk_option(-canvas) delete $position_mark_
      }
      if { $ref_position_mark_ != {} } {
         $itk_option(-canvas) delete $ref_position_mark_
      }
      remove_spectral_bindings_
   }

   #  Methods:
   #  --------

   #  Close window. If image is being replaced set dispsection false to
   #  avoid the section being loaded.
   public method close {} {
      stop_
      wm withdraw $w_

      if { $spectrum_ != {} && [winfo exists $spectrum_] } {
         $spectrum_ close
      }
      if { $position_mark_ != {} } {
         $itk_option(-canvas) delete $position_mark_
         set position_mark_ {}
      }
      if { $ref_position_mark_ != {} } {
         $itk_option(-canvas) delete $ref_position_mark_
         set ref_position_mark_ {}
      }
      remove_spectral_bindings_
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
         set_step_axis_ 3

         #  Display spectra on mouse click.
         add_bindings_

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

         $itk_component(lower) configure -from $plane_min_ -to $plane_max_
         $itk_component(upper) configure -from $plane_min_ -to $plane_max_

         $itk_component(collower) configure -from $plane_min_ -to $plane_max_
         $itk_component(colupper) configure -from $plane_min_ -to $plane_max_

         $itk_component(lower) configure -value $plane_min_
         set_animate_lower_bound_ $plane_min_
         $itk_component(upper) configure -value $plane_max_
         set_animate_upper_bound_ $plane_max_

         $itk_component(collower) configure -value $plane_min_
         set_collapse_lower_bound_ $plane_min_
         $itk_component(colupper) configure -value $plane_max_
         set_collapse_upper_bound_ $plane_max_

         #  Update the display of label and units in index component.
         $itk_component(index) update_coords_type $plane_

         #  Set up object to control units.
         $spec_coords_ configure -axis $axis_ -accessor $cubeaccessor_

         #  Set an initial plane, if requested.
         if { $setplane } {
            set plane_ $plane_min_
            set_display_plane_ [expr ($plane_max_ + $plane_min_)/2] 1
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

      if { $spectrum_ != {} } {
         if { $last_cxcy_ != {} } {
            eval display_spectrum_ localstart $last_cxcy_
         }
      }
   }

   #  Set the plane to display and display it. When regen is true a new
   #  image NDF is displayed, otherwise just the NDF image data is updated.
   protected method set_display_plane_ { newvalue {regen 0} } {

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

      #  If regenerating create the dummy NDF.
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
         display_ $section_name_ 0

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
      $itk_option(-rtdimage) object "[slice_display_name_] ($object_)"
      $rtdctrl_info_ updateValues

      # Release memory from last time and save pointer.
      if { $last_slice_adr_ != 0 } {
         $cubeaccessor_ release $last_slice_adr_
      }
      set last_slice_adr_ $adr

      #  Make sure position show in slide matches this (note feedback is
      #  avoided as $newvalue != $plane_).
      $itk_component(index) configure -value $plane_
   }

   #  Set the position of a spectral reference line.
   public method set_spec_ref_coord {id coord} {
      if { $spectrum_ != {} && $coord != {} } {
         $spectrum_ set_ref_line_coord $id $coord
      }
   }

   #  Handle the change in a spectral reference line, when done within the
   #  plot (user interaction by dragging line).
   protected method ref_line_moved_ {id coord action} {

      #  Convert coord from grid indices to pixel indices.
      set coord [axis_grid2pixel_ $coord]

      #  Apply value to the right control, but avoiding new actions being
      #  triggered, unless this is release when we want to reposition to the
      #  exact control feedback.
      if { $action == "move" } {
         set oldvalue [$index_control_($id) cget -show_ref_line]
         $index_control_($id) configure -show_ref_line 0
      }
      $index_control_($id) configure -value $coord

      #  And get the associated action to run.
      eval [$index_control_($id) cget -coord_update_cmd] $coord

      #  If action is released also do the drag_update_cmd.
      if { $action == "released" } {
         eval [$index_control_($id) cget -drag_update_cmd]
      }
      if { $action == "move" } {
         $index_control_($id) configure -show_ref_line $oldvalue
      }
   }

   #  Update the dummy NDF WCS so that it matches the current spectral
   #  coordinates. This should be done only when necessary (end of animation,
   #  slice slider drag etc.).
   public method update_wcs_ {} {
      set index [axis_pixel2grid_ $plane_]
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

   #  Display an image.
   protected method display_ {name {istemp 0} } {
      $itk_option(-gaia) configure -check_for_cubes 0
      $itk_option(-gaia) open $name
      $itk_option(-gaia) configure -temporary $istemp
      $itk_option(-gaia) configure -check_for_cubes $check_for_cubes_
   }

   #  Set the animation lower bound.
   protected method set_animate_lower_bound_ {bound} {
      set lower_animate_bound_ $bound
   }

   #  Set the animation upper bound.
   protected method set_animate_upper_bound_ {bound} {
      set upper_animate_bound_ $bound
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

   #  Start the animation.
   protected method start_ {} {
      set initial_seconds_ [clock clicks -milliseconds]
      if { $afterId_ == {} } {
         if { $lower_animate_bound_ > $upper_animate_bound_ } {
            set temp $lower_animate_bound_
            set lower_animate_bound_ $upper_animate_bound_
            set upper_animate_bound_ $temp
         }
         set step_ $itk_option(-step)
         set_display_plane_ $lower_animate_bound_ 0
         increment_
      }
   }
   protected variable initial_seconds_ 0

   #  Stop the animation.
   protected method stop_ {} {
      if { $afterId_ != {} } {
         after cancel $afterId_
         set afterId_ {}
         # DEBUG
         puts "animated for: [expr [clock clicks -milliseconds] - $initial_seconds_]"

         #  Update the WCS so that the spectral axis coordinate is correct.
         update_wcs_
      }
   }

   #  Set the animation delay.
   protected method set_delay_ {delay} {
      if { $delay <= 0 } {
         configure -delay 1
      } else {
         configure -delay $delay
      }
   }

   #  Set the animation step.
   protected method set_step_ {step} {
      configure -step $step
   }

   #  Increment the displayed section by one.
   protected method increment_ {} {
      if { $plane_ >= $lower_animate_bound_ && $plane_ < $upper_animate_bound_ } {
         set_display_plane_ [expr ${plane_}+$step_] 0
         if { $plane_ == $lower_animate_bound_ } {
            #  At lower edge, running backwards, need to let it step below.
            set plane_ [expr ${plane_}+$step_]
         }
         set afterId_ [after $itk_option(-delay) [code $this increment_]]
      } else {
         #  Off end so stop, or loop back to beginning, or go into reverse
         #  with rock 'n roll option.
         #  Check that we have a range, otherwise this will call increment_
         #  causing an eval depth exception.
         if { $lower_animate_bound_ == $upper_animate_bound_ } {
            stop_
         } else {
            #  Force temporary halt as visual clue that end has arrived.
            update idletasks
            after 500
            if { $loop_ != "off" } {
               if { $loop_ != "on" } {
                  #  Rock 'n roll, switch direction.
                  if { $step_ >= 1 } {
                     # Going up.
                     set plane_ [expr $upper_animate_bound_ - 1]
                  } else {
                     # Going down.
                     set plane_ $lower_animate_bound_
                  }
                  set step_ [expr -1*$step_]
               } else {
                  set plane_ $lower_animate_bound_
                  #  Increment is always positive, put may be changed on fly.
                  set step_ [expr abs($step_)]
               }
               increment_
            } else {
               stop_
            }
         }
      }
   }

   #  Set the collapse lower bound.
   protected method set_collapse_lower_bound_ {bound} {
      set lower_collapse_bound_ $bound
   }

   #  Set the collapse upper bound.
   protected method set_collapse_upper_bound_ {bound} {
      set upper_collapse_bound_ $bound
   }

   # Set the combination type
   protected method set_combination_type_ {type} {
      set combination_type_ $type
   }

   #  Collapse image and the display the result.
   #  Use a section to pass to COLLAPSE so we do not need to know the world
   #  coordinates.
   protected method collapse_ {} {
      set range "$lower_collapse_bound_:$upper_collapse_bound_"
      if { $axis_ == 1 } {
         set section "($range,,$close_section_"
      } elseif { $axis_ == 2 } {
         set section "(,$range,$close_section_"
      } else {
         set section "(,,${range}${close_section_}"
      }

      #  Now startup the COLLAPSE application.
      if { $collapser_ == {} } {
         global env
         set collapser_ [GaiaApp \#auto -application \
                            $env(KAPPA_DIR)/collapse \
                            -notify [code $this collapse_completed_]]
      }

      #  Create a temporary file name.
      set tmpimage_ "GaiaTempCollapse${count_}"
      incr count_

      blt::busy hold $w_
      $collapser_ runwiths "in=${ndfname_}$section out=$tmpimage_ axis=$axis_ \
                            estimator=$combination_type_ accept"

      #  If the reference lines are displayed these need removing.
      set show_collapse_lines_ 0
      toggle_show_collapse_lines_
   }

   #  Display a collapsed image.
   private method collapse_completed_ {} {
      set file {}
      if { ! [file readable $tmpimage_] } {
         if { ! [file readable ${tmpimage_}.sdf] } {
            blt::busy release $w_
            return
         }
         set file ${tmpimage_}.sdf
      } else {
         set file $tmpimage_
      }
      if { $file != {} } {
         display_ $file 1
      }
      blt::busy release $w_
   }

   #  Configure canvas so we get any clicks on the image and can display
   #  the associated spectra.
   protected method add_bindings_ {} {

      #  Bindings for sending to SPLAT (or not).
      set_splat_bindings_

      #  Bindings for GaiaSpectralPlot instance.
      add_spectral_bindings_
   }

   #  Add or disable SPLAT binding. XXX move SPLAT behaviour into
   #  GaiaSpectralPlot as a more obvious control.
   protected method set_splat_bindings_ {} {
      global env
      if { [info exists env(SPLAT_DIR)] && $itk_option(-use_splat)} {
         #  Double click sends to SPLAT.
         set splat_dir_ $env(SPLAT_DIR)
         $itk_option(-canvas) bind all <Double-Button-1> \
            [code $this handle_double_click_ splat %x %y]
      } else {
         #  Double click defines a reference spectrum.
         $itk_option(-canvas) bind all <Double-Button-1> \
            [code $this handle_double_click_ ref %x %y]
      }
   }


   #  Add bindings to rtdimage in the main canvas for spectral plot.
   #  These are single-click and drag-click on the image and cross.
   protected method add_spectral_bindings_ {} {

      $itk_option(-canvas) bind $itk_option(-rtdimage) <1> \
         [code $this display_spectrum_ localstart %x %y]

      $itk_option(-canvas) bind $itk_option(-rtdimage)  <B1-Motion> \
         [code $this display_spectrum_ localdrag %x %y]
   }

   #  Remove bindings from main canvas for spectral plot.
   protected method remove_spectral_bindings_ {} {
      $itk_option(-canvas) bind $itk_option(-rtdimage) <1> {}
      $itk_option(-canvas) bind $itk_option(-rtdimage) <B1-Motion> {}
      $itk_option(-canvas) bind all <Double-Button-1> {}
   }

   #  Display a spectrum in the local plot. Action can be "localstart" or
   # "localdrag", to start a spectral display (sets the initial scale of a
   # drag), or update during a drag.
   protected method display_spectrum_ {action cx cy} {

      #  Retain coordinates for a possible update.
      set last_cxcy_ "$cx $cy"

      #  Convert click coordinates from canvas coords to grid coords.
      set ccx [$itk_option(-canvas) canvasx $cx]
      set ccy [$itk_option(-canvas) canvasy $cy]
      $itk_option(-rtdimage) convert coords $ccx $ccy canvas iix iiy image

      if { $spectrum_ == {} } {
         #  Need to create a spectrum plot. Note show short help in this
         #  window to save real estate.
         set spectrum_ [GaiaSpectralPlot $w_.specplot \
                           -number $itk_option(-number) \
                           -spec_coords [code $spec_coords_] \
                           -ref_changed_cmd [code $this ref_line_moved_] \
                           -shorthelpwin [scope $short_help_win_]]

         #  Make this a transient of main window, not this one.
         wm transient $spectrum_ $itk_option(-gaia)

         #  Create the marker for the image position.
         create_position_marker_ $ccx $ccy

      } else {

         #  Already have a plot, re-display if withdrawn.
         if { [wm state $spectrum_] == "withdrawn" } {
            $spectrum_ open
         }

         #  Re-create the marker for the image position.
         if { $position_mark_ == {} } {
            create_position_marker_ $ccx $ccy
         }
      }

      #  Correct collapse bounds to grid indices.
      set alow [axis_pixel2grid_ $lower_collapse_bound_]
      set ahigh [axis_pixel2grid_ $upper_collapse_bound_]

      #  Make sure ix and iy are integers (zoomed images).
      set ix [expr round($iix)]
      set iy [expr round($iiy)]

      #  Move position marker on the image.
      $itk_option(-canvas) coords $position_mark_ $ccx $ccy

      #  Also autoscale when single click, so that we are not fixed for
      #  all time.
      if { $action == "localstart" } {
         busy {
            $spectrum_ display $cubeaccessor_ $axis_ $alow $ahigh $ix $iy 1
         }

         #  Set first-time position of the main reference line.
         set coord [get_coord $plane_ 0 0]
         if { $coord != {} } {
            update ;# Make sure plot is created first.
            set_spec_ref_coord 1 $coord
         }
      } else {
         busy {
            $spectrum_ display $cubeaccessor_ $axis_ $alow $ahigh $ix $iy 0
         }
      }
   }

   #  Convert grid indices to pixel indices along image axes.
   protected method image_grid2pixel_ {x y} {
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
   protected method axis_grid2pixel_ { value } {
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
   protected method image_pixel2grid_ {x y} {
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
   protected method axis_pixel2grid_ { value } {
      if { $axis_ == 1 } {
         set zo [lindex $bounds_ 0]
      } elseif { $axis_ == 2 } {
         set zo [lindex $bounds_ 2]
      } else {
         set zo [lindex $bounds_ 4]
      }
      return [expr round($value+1-$zo)]
   }

   #  Deal with a double click. This defines a reference spectrum or sends the
   #  spectrum to SPLAT.
   protected method handle_double_click_ {dest cx cy} {

      #  Convert click coordinates from canvas coords to grid coords.
      set ccx [$itk_option(-canvas) canvasx $cx]
      set ccy [$itk_option(-canvas) canvasy $cy]
      $itk_option(-rtdimage) convert coords $ccx $ccy canvas iix iiy image

      if { $dest == "ref" } {
         if { $spectrum_ == {} } {
            return
         }

         #  Already have a plot, re-display if withdrawn.
         if { [wm state $spectrum_] == "withdrawn" } {
            $spectrum_ open
         }

         #  Correct collapse bounds to grid indices.
         set alow [axis_pixel2grid_ $lower_collapse_bound_]
         set ahigh [axis_pixel2grid_ $upper_collapse_bound_]

         #  Make sure ix and iy are integers (zoomed images).
         set ix [expr round($iix)]
         set iy [expr round($iiy)]

         #  Display it as a reference spectrum.
         busy {
            $spectrum_ display_reference $cubeaccessor_ $axis_ $alow $ahigh \
               $ix $iy
         }

         #  Re-create the marker for the image position, if needed.
         if { $ref_position_mark_ == {} } {
            create_ref_position_marker_ $ccx $ccy
         }

         #  Move position marker on the image.
         $itk_option(-canvas) coords $ref_position_mark_ $ccx $ccy

      } else {

         #  Correct to pixel indices.
         lassign [image_grid2pixel_ $iix $iiy] ix iy

         #  Create the right section. Use collapse coords as bounds on the
         #  spectral axis.
         set range "$lower_collapse_bound_:$upper_collapse_bound_"
         if { $axis_ == 1 } {
            set section "($range,$ix,${iy}${close_section_}"
         } elseif { $axis_ == 2 } {
            set section "($ix,$range,${iy}${close_section_})"
         } else {
            set section "($ix,$iy,${range}${close_section_}"
         }

         #  Send the section to SPLAT.
         if { $splat_disp_ == {} } {
            set splat_disp_ [GaiaForeignExec \#auto \
                                -application $splat_dir_/splatdisp \
                                -show_output 0]
         }
         $splat_disp_ runwith "${ndfname_}${section}" 0
      }
   }

   #  Create the main spectral line position marker.
   #  XXX refactor into GaiaSpectralPlot. Should be same colour as spectral
   #  line.
   protected method create_position_marker_ { cx cy } {

      #  Note fixscale so that always same size, regardless of zoom.
      set position_mark_ [$itk_option(-canvas) create rtd_mark \
                             $cx $cy -type cross -scale 1 \
                             -fixscale 1 -size 7 -outline blue]

      #  Bindings to move and select this.
      $itk_option(-canvas) bind $position_mark_ <1> \
         [code $this display_spectrum_ localstart %x %y]

      $itk_option(-canvas) bind $position_mark_  <B1-Motion> \
         [code $this display_spectrum_ localdrag %x %y]
   }

   #  Create the reference spectral position marker.
   protected method create_ref_position_marker_ { cx cy } {

      #  Note fixscale so that always same size, regardless of zoom.
      set ref_position_mark_ [$itk_option(-canvas) create rtd_mark \
                                 $cx $cy -type cross -scale 1 \
                                 -fixscale 1 -size 7 -outline green]

      #  Bindings so that main position mark moves when over this.
      $itk_option(-canvas) bind $ref_position_mark_ <1> \
         [code $this display_spectrum_ localstart %x %y]

      $itk_option(-canvas) bind $ref_position_mark_  <B1-Motion> \
         [code $this display_spectrum_ localdrag %x %y]
   }

   #  XXX Devel code for region spectra (was in display_spectrum_).
   #  if { $ardspectra_ == {} } {
   #     global gaia_dir
   #     set ardspectra_ [GaiaApp \#auto -application $gaia_dir/ardspectra]
   #  }
   #  set arddesc "CIRCLE($ix,$iy,20)"
   #  $ardspectra_  runwiths "in=$ndfname_ fixorigin=t region=\"$arddesc\" out=GaiaArdSpectrum"
   #  catch {
   #     $splat_disp_ runwith "GaiaArdSpectrum"
   #  } msg

   #  Toggle the display of the collapse reference lines.
   protected method toggle_show_collapse_lines_ {} {
      $itk_component(collower) configure -show_ref_line $show_collapse_lines_
      $itk_component(colupper) configure -show_ref_line $show_collapse_lines_
      if { $spectrum_ != {} } {
         if { $show_collapse_lines_ } {
            $spectrum_ make_ref_line 4
            $spectrum_ make_ref_line 5
            $spectrum_ set_ref_line_colour 4 "cyan"
            $spectrum_ set_ref_line_colour 5 "cyan"
            $itk_component(collower) configure -value $lower_collapse_bound_
            $itk_component(colupper) configure -value $upper_collapse_bound_
         } else {
            $spectrum_ remove_ref_line 4
            $spectrum_ remove_ref_line 5
         }
      }
   }

   #  Toggle the display of the animation reference lines.
   protected method toggle_show_animation_lines_ {} {
      $itk_component(lower) configure -show_ref_line $show_animation_lines_
      $itk_component(upper) configure -show_ref_line $show_animation_lines_
      if { $spectrum_ != {} } {
         if { $show_animation_lines_ } {
            $spectrum_ make_ref_line 2
            $spectrum_ make_ref_line 3
            $spectrum_ set_ref_line_colour 2 "yellow"
            $spectrum_ set_ref_line_colour 3 "yellow"
            $itk_component(lower) configure -value $lower_animate_bound_
            $itk_component(upper) configure -value $upper_animate_bound_
         } else {
            $spectrum_ remove_ref_line 2
            $spectrum_ remove_ref_line 3
         }
      }
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

   #  The canvas. Used for displaying spectra.
   itk_option define -canvas canvas Canvas {}

   #  The rtdimage instance.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Filters for selecting files.
   itk_option define -filter_types filter_types Filter_types {}

   #  The animation delay (ms).
   itk_option define -delay delay Delay 100

   #  The animation step defined in interface.
   itk_option define -step step Step 1 {
      set step_ $itk_option(-step)
   }

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

   #  Animation bounds.
   protected variable lower_animate_bound_ 0
   protected variable upper_animate_bound_ 0

   #  Collapse bounds.
   protected variable lower_collapse_bound_ 0
   protected variable upper_collapse_bound_ 0

   #  The COLLAPSE task.
   protected variable collapser_ {}

   #  Combination method.
   protected variable combination_type_ "Mean"

   #  The current axis.
   protected variable axis_ 1

   #  Id of the animation thread.
   protected variable afterId_ {}

   #  Name of the temporary image just created.
   protected variable tmpimage_

   #  The SPLAT home directory.
   protected variable splat_dir_ {}

   #  Task controller for splatdisp command.
   protected variable splat_disp_ {}

   #  Task controller for ardspectra
   protected variable ardspectra_ {}

   #  How animation loops. Off by default.
   protected variable loop_ "off"

   #  The current value of step during an animation.
   protected variable step_ 1

   #  Check for cubes setting of GAIA.
   protected variable check_for_cubes_ 1

   #  The spectrum plot item.
   protected variable spectrum_ {}

   #  The position marker that corresponds to the spectrum.
   protected variable position_mark_ {}

   #  The position marker that corresponds to the reference spectrum.
   protected variable ref_position_mark_ {}

   #  The name for the dummy NDF, with updatable image section.
   protected variable section_name_ ""

   #  Memory used for last slice. Free this when not needed.
   protected variable last_slice_adr_ 0

   #  The terminator characters for closing a section. May specify
   #  a final redundant axis.
   protected variable close_section_ ")"

   #  The object value set in the cube (used in slice object).
   protected variable object_ {}

   #  GaiaImagePanel object used in the main window. Forced to update to
   #  reveal the slice information.
   protected variable rtdctrl_info_ {}

   #  Object for controlling the spectral coordinates.
   protected variable spec_coords_ {}

   #  Last cx and cy values used in to open a spectrum.
   protected variable last_cxcy_ {}

   #  Whether to display the limits of the animation as reference lines in the
   #  plot.
   protected variable show_animation_lines_ 0

   #  Whether to display the limits of the collapse as reference lines in the
   #  plot.
   protected variable show_collapse_lines_ 0

   #  Array of index-based controls. These are indexed by their ref_id.
   protected variable index_control_

   #  Common variables: (shared by all instances)
   #  -----------------

   #  All the known collapse estimators, short and long descriptions.

   common estimators_ {
      Mean Mean
      WMean {Weighted Mean}
      Mode Mode
      Median Median
      Absdev {Mean absolute deviation}
      Comax {Co-ordinate of the maximum value}
      Comin {Co-ordinate of the minimum value}
      Integ {Integrated value}
      Iwc {Intensity-weighted co-ordinate}
      Iwd {Intensity-weighted dispersion}
      Max Maximum
      Min Minimum
      Rms RMS
      Sigma {Standard deviation}
      Sum Sum}

   #  The temporary image count.
   common count_ 0

#  End of class definition.
}

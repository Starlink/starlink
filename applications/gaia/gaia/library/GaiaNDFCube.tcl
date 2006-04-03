#+
#  Name:
#     GaiaNDFCube

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Control the display of an NDF cube as a series of images in GAIA.

#  Description:
#     This class opens a 3D NDF and then displays image shaped sections of it
#     in an associated window.

#  Invocations:
#
#        GaiaNDFCube object_name [configuration options]
#
#     This creates an instance of a GaiaNDFCube object. The return is
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

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     08-OCT-2004 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaNDFCube {}

itcl::class gaia::GaiaNDFCube {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Enable devel code.
      global env
      if { [info exists env(GAIA_DEVEL)] } {
         set spectrum_ {}
      }

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      set lwidth 20

      #  Set window properties.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]
      wm title $w_ "Display image sections of NDF cube ($itk_option(-number))"

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

      #  If file mapping is not available, then set value off.
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

      #  Whether to constantly update the spectrum data limits.
      $Options add checkbutton -label "Autoscale" \
         -variable [scope itk_option(-autoscale)] \
         -onvalue 1 \
         -offvalue 0
      add_menu_short_help $Options {Autoscale}  \
         {Continuously change data limits,
            otherwise fixed by last click (faster)}

      #  Add window help.
      add_help_button ndfcube "On Window..."
      add_short_help $itk_component(menubar).help \
         {Help menu: get some help about this window}

      #  Name of input NDF.
      itk_component add ndfcube {
         LabelFileChooser $w_.ndfcube \
            -labelwidth $lwidth \
            -text "Input NDF cube:" \
            -textvariable [scope itk_option(-ndfcube)] \
            -command [code $this set_chosen_ndf_] \
            -filter_types $itk_option(-filter_types)
      }
      pack $itk_component(ndfcube) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(ndfcube) \
         {Name of the input NDF, must be a cube}

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
         LabelEntryScale $w_.index \
            -text {Index of plane:} \
            -value $plane_ \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -from 1 \
            -to 100 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_display_plane_]
      }
      pack $itk_component(index) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(index) \
         {Index of the image plane to display (along current axis)}

      #  Button release on scale part of widget updates the image to a proper
      #  section. Return in entry part does same.
      $itk_component(index) bindscale <ButtonRelease-1> \
         [code $this display_current_section_]
      $itk_component(index) bindentry <Return> \
         [code $this display_current_section_]

      #  Index coordinate.
      itk_component add indexlabel {
         LabelValue $w_.indexlabel \
            -text {Coordinate of plane:} \
            -labelwidth $lwidth
      }
      pack $itk_component(indexlabel) -side top -fill x

      #  Index coordinate type.
      itk_component add indextype {
         LabelValue $w_.indextype \
            -text {Coordinate type:} \
            -labelwidth $lwidth
      }
      pack $itk_component(indextype) -side top -fill x

      #  Add tab window for choosing either the animation or collapse
      #  controls.
      itk_component add tabnotebook {
         iwidgets::tabnotebook $w_.tab \
            -angle 0 -tabpos n -width 350 -height 320
      }
      pack $itk_component(tabnotebook) -fill both -expand 1

      $itk_component(tabnotebook) add -label Animation
      set animationTab [$itk_component(tabnotebook) childsite 0]

      $itk_component(tabnotebook) add -label Collapse
      set collapseTab [$itk_component(tabnotebook) childsite 1]

      #  Animation section. Choose upper and lower limits and then set it
      #  away, need to loop around...
      itk_component add aruler {
         LabelRule $animationTab.aruler -text "Animation controls:"
      }
      pack $itk_component(aruler) -side top -fill x

      itk_component add lower {
         LabelEntryScale $animationTab.lower \
            -text {Lower index:} \
            -value 1 \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -from 1 \
            -to 100 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_lower_bound_]
      }
      pack $itk_component(lower) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(lower) \
         {Lower index used during animation}

      #  Coordinate.
      itk_component add lowerlabel {
         LabelValue $animationTab.lowerlabel \
            -text {Coordinate:} \
            -labelwidth $lwidth
      }
      pack $itk_component(lowerlabel) -side top -fill x

      itk_component add upper {
         LabelEntryScale $animationTab.upper \
            -text {Upper index:} \
            -value 1 \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -from 1 \
            -to 100 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_upper_bound_]
      }
      pack $itk_component(upper) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(upper) \
         {Upper index used during animation}

      #  Coordinate.
      itk_component add upperlabel {
         LabelValue $animationTab.upperlabel \
            -text {Coordinate:} \
            -labelwidth $lwidth
      }
      pack $itk_component(upperlabel) -side top -fill x

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

      itk_component add animation {
         frame $animationTab.animation
      }
      pack $itk_component(animation) -side top -fill x -ipadx 1m -ipady 2m

      itk_component add stop {
         button $itk_component(animation).stop -text Stop \
            -command [code $this stop_ 1]
      }
      pack $itk_component(stop) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(stop) {Stop animation}

      itk_component add start {
         button $itk_component(animation).start -text Start \
            -command [code $this start_]
      }
      pack $itk_component(start) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(start) {Start animation}

      #  Use limits to create a collapsed image using KAPPA COLLAPSE.
      #  Use a section to pass to COLLAPSE so we do not need to know the world
      #  coordinates.

      itk_component add cruler {
         LabelRule $collapseTab.cruler -text "Collapse controls:"
      }
      pack $itk_component(cruler) -side top -fill x

      itk_component add collower {
         LabelEntryScale $collapseTab.collower \
            -text {Lower index:} \
            -value 1 \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -from 1 \
            -to 100 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_collapse_lower_bound_]
      }
      pack $itk_component(collower) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(collower) \
         {Lower index used for creating collapsed image}

      #  Coordinate.
      itk_component add collowerlabel {
         LabelValue $collapseTab.collowerlabel \
            -text {Coordinate:} \
            -labelwidth $lwidth
      }
      pack $itk_component(collowerlabel) -side top -fill x


      itk_component add colupper {
         LabelEntryScale $collapseTab.colupper \
            -text {Upper index:} \
            -value 1 \
            -labelwidth $lwidth \
            -valuewidth 20 \
            -from 1 \
            -to 100 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_collapse_upper_bound_]
      }
      pack $itk_component(colupper) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(colupper) \
         {Upper index used for creating collapsed image}

      #  Coordinate.
      itk_component add colupperlabel {
         LabelValue $collapseTab.colupperlabel \
            -text {Coordinate:} \
            -labelwidth $lwidth
      }
      pack $itk_component(colupperlabel) -side top -fill x

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

      #  Close and release NDF. Don't release the slice memory, that should be
      #  done safely by now, otherwise best left hanging.
      if { $accessor_ != {} } {
         $accessor_ close
         set accessor_ {}
      }

      #  Stop animation.
      stop_ 0

      #  Release collapser task.
      if { $collapser_ != {} } {
         catch {$collapser_ delete_sometime}
         set collapser_ {}
      }

      #  Close spectrum plot.
      if { [info exists spectrum_] } {
         if { $spectrum_ != {} && [winfo exists $spectrum_] } {
            $spectrum_ close
            remove_spectral_bindings_
         }
         if { $position_mark_ != {} } {
            $itk_option(-canvas) delete $position_mark_
         }
      }
   }

   #  Methods:
   #  --------

   #  Close window.
   public method close {} {
      stop_ 1
      wm withdraw $w_
      if { [info exists spectrum_] } {
         if { $spectrum_ != {} && [winfo exists $spectrum_] } {
            $spectrum_ close
            remove_spectral_bindings_
         }
         if { $position_mark_ != {} } {
            $itk_option(-canvas) delete $position_mark_
         }
         set position_mark_ {}
      }
   }

   #  Open the chosen NDF.
   protected method set_chosen_ndf_ { args } {
      set namer [GaiaImageName \#auto -imagename $itk_option(-ndfcube)]
      if { [$namer exists] } {
         if { $accessor_ == {} } {
            set accessor_ [uplevel \#0 GaiaNDAccess \#auto]
         }

         $namer absolute
         set ndfname_ [$namer ndfname 0]
         $accessor_ configure -dataset "$ndfname_"
         set bounds_ [$accessor_ bounds]
         if { [llength $bounds_] != 6 } {
            set ndims [expr [llength $bounds_]/2]
            error_dialog "Not a cube, must have 3 dimensions (has $ndims)"
            return
         }
         set axis_ 2
         set_step_axis_ 3

         #  Display spectra on mouse click.
         add_bindings_

         #  Map in all data, this makes it immediately available within
         #  application
         load_cube_ 1
      }
   }

   #  Load the cube data, using the current mmap mode. Only happens when force
   #  is true, or if a change the mmap mode of the cube is needed.
   #  Controlling the mmap mode can force file i/o to be is used to load all
   #  the cube into malloc'd memory, which speeds the spectral
   #  readout. Otherwise mmap is used, which may give an initially slow
   #  spectral readout for large cubes.
   protected method load_cube_ { force } {
      if { $accessor_ == {} } {
         return
      }
      if { [$accessor_ cget -usemmap] != $itk_option(-usemmap) || $force } {
         busy {
            $accessor_ unmap
            $accessor_ configure -usemmap $itk_option(-usemmap)
            lassign [$accessor_ map] adr nel type
         }
      }
   }

   #  Set the axis we step along.
   protected method set_step_axis_ {value} {
      if { $value != $axis_ && $bounds_ != {} } {
         set axis_ $value
         set plane_min_ [lindex $bounds_ [expr (${axis_}-1)*2]]
         set plane_max_ [lindex $bounds_ [expr (${axis_}-1)*2+1]]

         $itk_component(index) configure -from $plane_min_ -to $plane_max_
         $itk_component(index) configure -value $plane_min_

         $itk_component(lower) configure -from $plane_min_ -to $plane_max_
         $itk_component(upper) configure -from $plane_min_ -to $plane_max_

         $itk_component(collower) configure -from $plane_min_ -to $plane_max_
         $itk_component(colupper) configure -from $plane_min_ -to $plane_max_

         $itk_component(lower) configure -value $plane_min_
         set_lower_bound_ $plane_min_
         $itk_component(upper) configure -value $plane_max_
         set_upper_bound_ $plane_max_

         $itk_component(collower) configure -value $plane_min_
         set_collapse_lower_bound_ $plane_min_
         $itk_component(colupper) configure -value $plane_max_
         set_collapse_upper_bound_ $plane_max_

         #  Label and units.
         set vlu [get_coord_ $plane_ 1 1]
         set trail [lassign $vlu value]
         $itk_component(indextype) configure -value $trail

         set plane_ $plane_min_
         set_display_plane_ [expr ( $plane_max_ + $plane_min_ ) / 2] 1
      }
   }

   #  Set the plane to display and display it. When regen is true a new slice
   #  is displayed, otherwise just the image data is updated.
   protected method set_display_plane_ { newvalue {regen 0} } {

      if { $newvalue != $plane_ && $ndfname_ != {} } {
         if { $newvalue >= $plane_max_ } {
            set plane_ $plane_max_
         } elseif { $newvalue <= $plane_min_ } {
            set plane_ $plane_min_
         } else {
            set plane_ $newvalue
         }

         if { $regen || ![info exists spectrum_] } {
            #  Display a section from the NDF (forced in non-devel mode).
            display_current_section_

         } else {
            #  Just extract this plane from the cube and update the image.
            #  Correct plane to grid indices.
            if { $axis_ == 1 } {
               set zo [lindex $bounds_ 0]
            } elseif { $axis_ == 2 } {
               set zo [lindex $bounds_ 2]
            } else {
               set zo [lindex $bounds_ 4]
            }
            set zp [expr round($plane_+1-$zo)]
            lassign [$accessor_ getimage $axis_ $zp] adr nel type
            $itk_option(-rtdimage) updateimagedata $adr

            # Release memory from last time and save pointer.
            if { $last_slice_adr_ != 0 } {
               $accessor_ release $last_slice_adr_
            }
            set last_slice_adr_ $adr
         }
         set coord [get_coord_ $plane_ 1 0]
         $itk_component(indexlabel) configure -value $coord

         #  Make sure position show in slide matches this (note feedback is
         #  avoided as $newvalue == $plane_).
         $itk_component(index) configure -value $plane_

         #  Move spectral reference position.
         if { [info exists spectrum_] } {
            if { $spectrum_ != {} } {
               set coord [get_coord_ $plane_ 0 0]
               if { $coord != {} } {
                  $spectrum_ set_ref_coord $coord
               }
            }
         }
      }
   }

   #  Display the current NDF section and release the memory slice.
   protected method display_current_section_ {} {

      if { $axis_ == 1 } {
         set section "($plane_,,)"
      } elseif { $axis_ == 2 } {
         set section "(,$plane_,)"
      } else {
         set section "(,,$plane_)"
      }
      display_ ${ndfname_}$section

      if { $last_slice_adr_ != 0 } {
         $accessor_ release $last_slice_adr_
         set last_slice_adr_ 0
      }
   }

   #  Display an NDF.
   protected method display_ {name {istemp 0} } {
      $itk_option(-gaia) configure -check_for_cubes 0
      $itk_option(-gaia) open $name
      $itk_option(-gaia) configure -temporary $istemp
      $itk_option(-gaia) configure -check_for_cubes $check_for_cubes_
   }

   #  Set the animation lower bound.
   protected method set_lower_bound_ {bound} {
      set lower_bound_ $bound
      $itk_component(lowerlabel) configure -value [get_coord_ $bound 1 0]
   }

   #  Set the animation upper bound.
   protected method set_upper_bound_ {bound} {
      set upper_bound_ $bound
      $itk_component(upperlabel) configure -value [get_coord_ $bound 1 0]
   }

   #  Get the coordinate of an index along the current axis.
      protected method get_coord_ {index {formatted 1} {trail 0}} {
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
         set coord [$accessor_ coord $axis_ $section $formatted $trail]
      }
      return $coord
   }

   #  Start the animation.
   protected method start_ {} {
      set initial_seconds_ [clock clicks -milliseconds]
      if { $afterId_ == {} } {
         if { $lower_bound_ > $upper_bound_ } {
            set temp $lower_bound_
            set lower_bound_ $upper_bound_
            set upper_bound_ $temp
         }
         set step_ $itk_option(-step)
         set_display_plane_ $lower_bound_
         increment_
      }
   }
   protected variable initial_seconds_ 0

   #  Stop the animation. If dispsection is true display the current NDF
   #  section and release slice memory.
   protected method stop_ { {dispsection 0} } {

      if { $afterId_ != {} } {
         after cancel $afterId_
         set afterId_ {}
         puts "animated for: [expr [clock clicks -milliseconds] - $initial_seconds_]"
      }
      if { $dispsection } {
         display_current_section_
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
      if { $plane_ >= $lower_bound_ && $plane_ < $upper_bound_ } {
         set_display_plane_ [expr ${plane_}+$step_]
         if { $plane_ == $lower_bound_ } {
            #  At lower edge, running backwards, need to let it step below.
            set plane_ [expr ${plane_}+$step_]
         }
         set afterId_ [after $itk_option(-delay) [code $this increment_]]
      } else {
         #  Off end so stop, or loop back to beginning, or go into reverse
         #  with rock 'n roll option.
         #  Check that we have a range, otherwise this will call increment_
         #  causing an eval depth exception.
         if { $lower_bound_ == $upper_bound_ } {
            stop_ 1
         } else {
            #  Force temporary halt as visual clue that end has arrived.
            update idletasks
            after 500
            if { $loop_ != "off" } {
               if { $loop_ != "on" } {
                  #  Rock 'n roll, switch direction.
                  if { $step_ >= 1 } {
                     # Going up.
                     set plane_ [expr $upper_bound_ - 1]
                  } else {
                     # Going down.
                     set plane_ $lower_bound_
                  }
                  set step_ [expr -1*$step_]
               } else {
                  set plane_ $lower_bound_
                  #  Increment is always positive, put may be changed on fly.
                  set step_ [expr abs($step_)]
               }
               increment_
            } else {
               stop_ 1
            }
         }
      }
   }

   #  Set the collapse lower bound.
   protected method set_collapse_lower_bound_ {bound} {
      set lower_collapse_bound_ $bound
      $itk_component(collowerlabel) configure -value [get_coord_ $bound 1 0]
   }

   #  Set the collapse upper bound.
   protected method set_collapse_upper_bound_ {bound} {
      set upper_collapse_bound_ $bound
      $itk_component(colupperlabel) configure -value [get_coord_ $bound 1 0]
   }

   # Set the combination type
   protected method set_combination_type_ {type} {
      set combination_type_ $type
   }

   #  Collapse image and the display the result.
   protected method collapse_ {} {
      set range "$lower_collapse_bound_:$upper_collapse_bound_"
      if { $axis_ == 1 } {
         set section "($range,,)"
      } elseif { $axis_ == 2 } {
         set section "(,$range,)"
      } else {
         set section "(,,$range)"
      }

      #  Now startup the COLLAPSE application.
      if { $collapser_ == {} } {
         global env
         set collapser_ [GaiaApp \#auto -application \
                            $env(KAPPA_DIR)/collapse \
                            -notify [code $this collapse_completed_]]
      }

      #  Create a temporary file name.
      set tmpimage_ "GaiaNDFCube${count_}"
      incr count_

      blt::busy hold $w_
      $collapser_ runwiths "in=${ndfname_}$section out=$tmpimage_ axis=$axis_ \
                            estimator=$combination_type_ accept"
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

      #  SPLAT bindings. May move this into GaiaSpectralPlot.
      global env
      if {! [info exists env(SPLAT_DIR)]} {
         puts stderr "Information: No SPLAT_DIR variable available. \
                      Cannot display spectra remotely"
      } else {
         set splat_dir_ $env(SPLAT_DIR)

         #  Button-1 does a lot already so use double click. May clash with
         #  image regions toolbox....
         $itk_option(-canvas) bind $itk_option(-rtdimage) <Double-Button-1> \
            [code $this display_spectrum_ splat %x %y]
      }

      #  Bindings for GaiaSpectralPlot instance.
      add_spectral_bindings_
   }

   #  Add bindings to main canvas for spectral plot.
   #  These are single-click and drag-click.
   protected method add_spectral_bindings_ {} {

      $itk_option(-canvas) bind all <B1-Motion> \
         [code $this display_spectrum_ localdrag %x %y]

      $itk_option(-canvas) bind all <1> \
         [code $this display_spectrum_ localstart %x %y]
   }

   #  Remove bindings from main canvas for spectral plot.
   protected method remove_spectral_bindings_ {} {
      $itk_option(-canvas) bind all <B1-Motion> {}
      $itk_option(-canvas) bind all <1> {}
   }

   #  Display a spectrum in the plot. Action can be "splat", "localstart" or
   #  "localdrag", for display in SPLAT, start a spectral display (sets the
   #  initial scale of a drag), or update during a drag.
   protected method display_spectrum_ {action cx cy} {

      #  Convert click coordinates from canvas coords to grid coords.
      set ccx [$itk_option(-canvas) canvasx $cx]
      set ccy [$itk_option(-canvas) canvasy $cy]
      $itk_option(-rtdimage) convert coords $ccx $ccy canvas iix iiy image

      if { $action == "splat" } {

         #  Use origins to get pixel indices.
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
         set ix [expr round($iix-1+$xo)]
         set iy [expr round($iiy-1+$yo)]

         #  Create the right section. Use collapse coords as bounds on the
         #  spectral axis.
         set range "$lower_collapse_bound_:$upper_collapse_bound_"
         if { $axis_ == 1 } {
            set section "($range,$ix,$iy)"
         } elseif { $axis_ == 2 } {
            set section "($ix,$range,$iy)"
         } else {
            set section "($ix,$iy,$range)"
         }

         #  Send the section to SPLAT.
         if { $splat_disp_ == {} } {
            set splat_disp_ [GaiaForeignExec \#auto \
                                -application $splat_dir_/splatdisp \
                                -show_output 0]
         }
         $splat_disp_ runwith "${ndfname_}${section}" 0

      } else {
         #  Display in the spectrum_plot. Note show short help in this window
         #  to save real estate.
         if { [info exists spectrum_] } {
            if { $spectrum_ == {} } {
               set spectrum_ [GaiaSpectralPlot $w_.specplot \
                                 -number $itk_option(-number) \
                                 -shorthelpwin [scope $short_help_win_]]

               #  Make this a transient of main window, not this one.
               wm transient $spectrum_ $itk_option(-gaia)

               #  Display in main window as well.
               $spectrum_ configure -canvas $itk_option(-canvas)

               #  Create the marker for the image position.
               create_position_marker_ $ccx $ccy
            } else {
               #  Re-display if withdrawn.
               if { [wm state $spectrum_] == "withdrawn" } {
                  $spectrum_ open
               }

               #  Re-create the marker for the image position.
               if { $position_mark_ == {} } {
                  create_position_marker_ $ccx $ccy
               }
            }

            #  Correct collapse bounds to grid indices.
            if { $axis_ == 1 } {
               set zo [lindex $bounds_ 0]
            } elseif { $axis_ == 2 } {
               set zo [lindex $bounds_ 2]
            } else {
               set zo [lindex $bounds_ 4]
            }
            set alow [expr round($lower_collapse_bound_+1-$zo)]
            set ahigh [expr round($upper_collapse_bound_+1-$zo)]

            #  Make sure ix and iy are integers (zoomed images).
            set ix [expr round($iix)]
            set iy [expr round($iiy)]

            #  Move position marker on the image.
            $itk_option(-canvas) coords $position_mark_ $ccx $ccy

            #  Also autoscale when single click, so that we are not fixed for
            #  all time.
            if { $itk_option(-autoscale) || $action == "localstart" } {
               busy {
                  $spectrum_ display $accessor_ $axis_ $alow $ahigh \
                     $ix $iy 1 $ccx $ccy
               }

               #  Set first-time reference position.
               set coord [get_coord_ $plane_ 0 0]
               if { $coord != {} } {
                  update; # Force redraw to get underlying plot right.
                  $spectrum_ set_ref_coord $coord
               }
            } else {
               busy {
                  $spectrum_ display $accessor_ $axis_ $alow $ahigh \
                     $ix $iy 0
               }
            }
         }
      }

      #  XXX Checker code for region spectra.
      #       if { $ardspectra_ == {} } {
      #          global gaia_dir
      #          set ardspectra_ [GaiaApp \#auto -application $gaia_dir/ardspectra]
      #       }
      #       set arddesc "CIRCLE($ix,$iy,20)"
      #       echo "$ardspectra_  runwiths in=$ndfname_ fixorigin=t region=\"$arddesc\" out=GaiaArdSpectrum"
      #       $ardspectra_  runwiths "in=$ndfname_ fixorigin=t region=\"$arddesc\" out=GaiaArdSpectrum"
      #       catch {
      #          echo "$splat_disp_ runwith GaiaArdSpectrum"
      #          $splat_disp_ runwith "GaiaArdSpectrum"
      #       } msg
      #       puts "msg = $msg"
   }

   #  Create the spectral position marker.
   #  XXX refactor into GaiaSpectralPlot. Should be same colour as reference
   #  line.
   protected method create_position_marker_ { cx cy } {

      #  Note fixscale so that always same size, regardless of zoom.
      set position_mark_ [$itk_option(-canvas) create rtd_mark \
                             $cx $cy -type cross -scale 1 \
                             -fixscale 1 -size 7 -outline red]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the input text file.
   itk_option define -ndfcube ndfcube Ndfcube {} {
      set_chosen_ndf_
   }

   #  Name of the Gaia instance we're controlling.
   itk_option define -gaia gaia Gaia {} {
      set check_for_cubes_ [$itk_option(-gaia) cget -check_for_cubes]
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

   #  Does spectral plot auto-update ranges.
   itk_option define -autoscale autoscale AutoScale 0

   #  Whether to use file mapping (quick startup), or direct io (fast
   #  spectral display).
   itk_option define -usemmap usemmap UseMmap 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Data access object.
   protected variable accessor_ {}

   #  The bounds of the NDF, 3 pairs of upper and lower values.
   protected variable bounds_ {}

   #  The name of the NDF.
   protected variable ndfname_ {}

   #  The current plane along the current axis.
   protected variable plane_ 1

   #  Maximum and minimum possible value for plane.
   protected variable plane_max_ 0
   protected variable plane_min_ 0

   #  Animation bounds.
   protected variable lower_bound_ 0
   protected variable upper_bound_ 0

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

   #  The spectrum plot item, XXX variable not initialised unless in
   #  development mode.
   protected variable spectrum_

   #  The position marker that corresponds to the spectrum.
   protected variable position_mark_ {}

   #  Memory used for last slice. Free this when not needed.
   protected variable last_slice_adr_ 0

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

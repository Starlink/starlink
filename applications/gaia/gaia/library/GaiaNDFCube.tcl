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

      #  Create the rtdimage instance used to access the NDF.
      set rtdimage_ [::image create rtdimage]

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

      #  Display spectra on mouse click.
      add_bindings_
   }

   #  Destructor:
   #  -----------
   destructor  {
      # Close and release NDF.
      if { $id_ != 0 } {
         $rtdimage_ ndf close $id_
      }

      #  Stop animation.
      stop_

      #  Release collapser task.
      if { $collapser_ != {} } {
         catch {$collapser_ delete_sometime}
         set collapser_ {}
      }

      #  Close spectrum plot.
      if { [info exists spectrum_] } {
         if { $spectrum_ != {} && [winfo exists $spectrum_] } {
            $spectrum_ close
         }
      }
   }

   #  Methods:
   #  --------

   #  Close window.
   public method close {} {
      stop_
      wm withdraw $w_
   }

   #  Open the chosen NDF.
   protected method set_chosen_ndf_ { args } {

      set namer [GaiaImageName \#auto -imagename $itk_option(-ndfcube)]
      if { [$namer exists] } {
         if { $id_ != 0 } {
            $rtdimage_ ndf close $id_
         }

         $namer absolute
         set ndfname_ [$namer ndfname 0]
         set id_ [$rtdimage_ ndf open $ndfname_]
         set bounds_ [$rtdimage_ ndf query $id_ bounds]
         if { [llength $bounds_] != 6 } {
            set ndims [expr [llength $bounds_]/2]
            error_dialog "Not a cube, must have 3 dimensions (has $ndims)"
            return
         }
         set axis_ 2
         set_step_axis_ 3
         set_display_plane_ $plane_

         #  XXX map in all data for extra speed... When to release?
         #set ndfid [ndf::open "$ndfname_"]
         #lassign [ndf::map $ndfid] adr nel type
         #puts "adr=$adr, nel=$nel, type=$type"
      }
   }

   #  Set the axis we step along.
   protected method set_step_axis_ {value} {
      if { $value != $axis_ && $bounds_ != {} } {
         set axis_ $value
         set plane_min_ [lindex $bounds_ [expr (${axis_}-1)*2]]
         set plane_max_ [lindex $bounds_ [expr (${axis_}-1)*2+1]]

         $itk_component(index) configure -from $plane_min_ -to $plane_max_

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
         set vlu [get_coord_ $plane_ 1]
         set trail [lassign $vlu value]
         $itk_component(indextype) configure -value $trail

         set plane_ $plane_min_
         set_display_plane_ [expr (${plane_max_}- ${plane_min_})/2]
      }
   }

   #  Set the plane to display and display it.
   protected method set_display_plane_ { newvalue } {
      if { $newvalue != $plane_ && $ndfname_ != {} } {
         if { $newvalue >= $plane_max_ } {
            set plane_ $plane_max_
         } elseif { $newvalue <= $plane_min_ } {
            set plane_ $plane_min_
         } else {
            set plane_ $newvalue
         }

         if { $axis_ == 1 } {
            set section "($plane_,,)"
         } elseif { $axis_ == 2 } {
            set section "(,$plane_,)"
         } else {
            set section "(,,$plane_)"
         }
         display_ ${ndfname_}$section
         $itk_component(indexlabel) configure -value [get_coord_ $plane_]
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
      $itk_component(lowerlabel) configure -value [get_coord_ $bound]
   }

   #  Set the animation upper bound.
   protected method set_upper_bound_ {bound} {
      set upper_bound_ $bound
      $itk_component(upperlabel) configure -value [get_coord_ $bound]
   }

   #  Get the coordinate of an index along the current axis.
   protected method get_coord_ {index {trail 0}} {
      if { $axis_ == 1 } {
         set section [list $index 1 1]
      } elseif { $axis_ == 2 } {
         set section [list 1 $index 1]
      } else {
         set section [list 1 1 $index]
      }
      set coord {}
      catch {
         set coord [$rtdimage_ ndf query $id_ coord $axis_ $section $trail]
      }
      return $coord
   }

   #  Start the animation.
   protected method start_ {} {
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

   #  Stop the animation.
   protected method stop_ {} {
      if { $afterId_ != {} } {
         after cancel $afterId_
         set afterId_ {}
      }
   }

   #  Set the animation delay.
   protected method set_delay_ {delay} {
      configure -delay $delay
   }

   #  Set the animation step.
   protected method set_step_ {step} {
      configure -step $step
   }

   #  Increment the displayed section by one.
   protected method increment_ {} {
      if { $plane_ >= $lower_bound_ && $plane_ < $upper_bound_ } {
         $itk_component(index) configure -value [expr ${plane_}+1]
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
               stop_
            }
         }
      }
   }

   #  Set the collapse lower bound.
   protected method set_collapse_lower_bound_ {bound} {
      set lower_collapse_bound_ $bound
      $itk_component(collowerlabel) configure -value [get_coord_ $bound]
   }

   #  Set the collapse upper bound.
   protected method set_collapse_upper_bound_ {bound} {
      set upper_collapse_bound_ $bound
      $itk_component(colupperlabel) configure -value [get_coord_ $bound]
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
   protected method add_bindings_ {} {
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

      # Local test...
      #$itk_option(-canvas) bind $itk_option(-rtdimage) <B1-Motion> \
      #   [code $this display_spectrum_ localdrag %x %y]

      #$itk_option(-canvas) bind $itk_option(-rtdimage) <1> \
      #   [code $this display_spectrum_ localstart %x %y]

      # Bindings when display over main image as well.
      $itk_option(-canvas) bind all <B1-Motion> \
         [code $this display_spectrum_ localdrag %x %y]

      $itk_option(-canvas) bind all <1> \
         [code $this display_spectrum_ localstart %x %y]

   }

   #  Display a spectrum in the plot. Action can be "splat", "localstart" or
   #  "localdrag", for display in SPLAT, start a spectral display (sets the
   #  initial scale of a drag), or update during a drag.
   protected method display_spectrum_ {action ccx ccy} {

      #  Convert click coordinates from canvas coords to grid coords.
      set cx [$itk_option(-canvas) canvasx $ccx]
      set cy [$itk_option(-canvas) canvasy $ccy]
      $itk_option(-rtdimage) convert coords $cx $cy canvas ix iy image

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
      set ix [expr round($ix-1+$xo)]
      set iy [expr round($iy-1+$yo)]

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

      if { $action == "splat" } {
         #  Send the section to SPLAT.
         if { $splat_disp_ == {} } {
            set splat_disp_ [GaiaForeignExec \#auto \
                                -application $splat_dir_/splatdisp \
                                -show_output 0]
         }
         $splat_disp_ runwith "${ndfname_}${section}" 0
      } else {
         #  Display in the spectrum_plot. Devel code. Add -canvas to display
         #  elsewhere.
         if { [info exists spectrum_] } {
            if { $spectrum_ == {} } {
               set spectrum_ [GaiaSpectralPlot $w_.specplot \
                                 -rtdimage $itk_option(-rtdimage) \
                                 -number $itk_option(-number)]
               #  Make this a transient of main window, not this one.
               wm transient $spectrum_ $itk_option(-gaia)

               #  Display in main window as well.
               $spectrum_ configure -canvas $itk_option(-canvas)
            }

            #  Also autoscale when single click, so that we are not fixed for
            #  all time.
            if { $itk_option(-autoscale) || $action == "localstart" } {
               $spectrum_ display "${ndfname_}${section}" $axis_ 1 $cx $cy
            } else {
               $spectrum_ display "${ndfname_}${section}" $axis_ 0
            }
         }
      }

      #  XXX Checker code for region spectra.
#       if { $ardspectra_ == {} } {
#          global gaia_dir
#          set ardspectra_ [GaiaApp \#auto -application $gaia_dir/ardspectra]
#       }
#       set arddesc "CIRCLE($ix,$iy,20)"
#       echo "$ardspectra_  runwiths \
#           in=$ndfname_ fixorigin=t region=\"$arddesc\" out=GaiaArdSpectrum"
#       $ardspectra_  runwiths \
#          "in=$ndfname_ fixorigin=t region=\"$arddesc\" out=GaiaArdSpectrum"

#       catch {
#          echo "$splat_disp_ runwith GaiaArdSpectrum"
#          $splat_disp_ runwith "GaiaArdSpectrum"
#       } msg
#       puts "msg = $msg"
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

   #  The rtdimage instance. Used for coordinate conversions.
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

   #  Protected variables: (available to instance)
   #  --------------------

   #  Local rtdimage instance, this opens the cube as an NDF.
   protected variable rtdimage_ {}

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

   #  The current NDF identifier.
   protected variable id_ 0

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

   # The spectrum plot item, variable not initialised unless in development
   # mode.
   protected variable spectrum_

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

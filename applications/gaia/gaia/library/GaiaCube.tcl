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

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

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
            -labelwidth 15 \
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
            -labelwidth 15
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
            -labelwidth 15 \
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

      #  Animation section. Choose upper and lower limits and then set it
      #  away, need to loop around...
      itk_component add aruler {
         LabelRule $w_.aruler -text "Animation controls:"
      }
      pack $itk_component(aruler) -side top -fill x

      itk_component add lower {
         LabelEntryScale $w_.lower \
            -text {Lower index:} \
            -value 1 \
            -labelwidth 15 \
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

      itk_component add upper {
         LabelEntryScale $w_.upper \
            -text {Upper index:} \
            -value 1 \
            -labelwidth 15 \
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

      itk_component add animation {
         frame $w_.animation
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


      #  Close window.
      itk_component add close {
         button $w_.close -text Close \
            -command [code $this close]
      }
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(close) {Close window}
   }

   #  Destructor:
   #  -----------
   destructor  {
      # Close and release NDF.
      if { $id_ != 0 } {
         $rtdimage_ ndf close $id_
      }
      stop_
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
         set_lower_bound_ $plane_min_
         set_upper_bound_ $plane_max_

         set plane_ $plane_min_
         set_display_plane_ [expr (${plane_max_}- ${plane_min_})/2]
      }
   }

   #  Set the plane to display.
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
      }
   }

   #  Display an NDF.
   protected method display_ {name} {
      $itk_option(-gaia) open $name
   }

   #  Set the animation lower bound.
   protected method set_lower_bound_ {bound} {
      set lower_bound_ $bound
   }

   #  Set the animation upper bound.
   protected method set_upper_bound_ {bound} {
      set upper_bound_ $bound
   }

   #  Start the animation.
   protected method start_ {} {
      if { $afterId_ == {} } {
         if { $lower_bound_ > $upper_bound_ } {
            set temp $lower_bound_
            set lower_bound_ $upper_bound_
            set upper_bound_ $temp
         }
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

   #  Increment the displayed section by one.
   protected method increment_ {} {
      if { $plane_ >= $lower_bound_ && $plane_ < $upper_bound_ } {
         $itk_component(index) configure -value [expr ${plane_}+1]
         set_display_plane_ [expr ${plane_}+1]
         set afterId_ [after $itk_option(-delay) [code $this increment_]]
      } else {
         #  Off end so stop.
         stop_
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the input text file.
   itk_option define -ndfcube ndfcube Ndfcube {} {
      set_chosen_ndf_
   }

   #  Name of the Gaia instance we're controlling.
   itk_option define -gaia gaia Gaia {}

   #  Filters for selecting files.
   itk_option define -filter_types filter_types Filter_types {}

   #  The animation delay (ms).
   itk_option define -delay delay Delay 100

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

   #  The current axis.
   protected variable axis_ 1

   #  The current NDF identifier.
   protected variable id_ 0
   
   #  Id of the animation thread.
   protected variable afterId_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

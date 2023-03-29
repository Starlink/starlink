#+
#  Name:
#     GaiaCubeFilter

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Controls for filtering the current cube.

#  Description:
#     This class creates a panel of controls for filtering a cube.
#     The new cube is loaded to replace the existing one.

#  Invocations:
#
#        GaiaCubeFilter object_name [configuration options]
#
#     This creates an instance of a GaiaCubeFilter object. The return is
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
#     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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
#     MJC: Malcolm J. Currie (JAC, Hawaii)
#     {enter_new_authors_here}

#  History:
#     07-FEB-2007 (PWD):
#        Original version.
#     2012 April 20 (MJC):
#        Added support for standard of rest.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeFilter {}

itcl::class gaia::GaiaCubeFilter {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Filter method, square, rectangle, gaussian or gaussian-elliptical.
      itk_component add filter {
         util::LabelMenu $w_.filter \
            -labelwidth $itk_option(-labelwidth) \
            -text "Filter method:" \
            -variable [scope filter_type_]
      }
      pack $itk_component(filter) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(filter) \
         {Method to use when filtering data}

      foreach name "square rectangle gaussian gaussian-elliptical" {
         $itk_component(filter) add \
            -label $name \
            -value $name \
            -command [code $this set_filter_type_ $name]
      }

      #  Size of smoothing rectangle or gaussian evaluation.
      itk_component add boxsize1 {
         util::LabelEntryScale $w_.boxsize1 \
            -text "Box size 1:" \
            -value $boxsize1_ \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 1 \
            -to 21 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_boxsize_ 1]
      }
      pack $itk_component(boxsize1) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(boxsize1) \
         {Size of box for block filtering or gaussian evaluation}

      itk_component add boxsize2 {
         util::LabelEntryScale $w_.boxsize2 \
            -text "Box size 2:" \
            -value $boxsize2_ \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 1 \
            -to 21 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_boxsize_ 2]
      }
      pack $itk_component(boxsize2) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(boxsize2) \
         {Size of box for block filtering or gaussian evaluation}

      #  Estimator for block filtering. Either mean or median.
      itk_component add combination {
         util::LabelMenu $w_.combination \
            -labelwidth $itk_option(-labelwidth) \
            -text "Estimator:" \
            -variable [scope combination_type_]
      }
      pack $itk_component(combination) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(combination) \
         {Method to use estimating value of a block}

      foreach name "mean median" {
         $itk_component(combination) add \
            -label $name \
            -value $name \
            -command [code $this set_combination_type_ $name]
      }

      #  Orientation of gaussian-elliptical.
      itk_component add orient {
         util::LabelEntryScale $w_.orient \
            -text "Orientation:" \
            -value $orient_ \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 0 \
            -to 180 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_orient_]
      }
      pack $itk_component(orient) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(orient) \
         {Orientation of major axis, degrees X through Y}

      #  FWHM of gaussian types.
      itk_component add fwhm1 {
         util::LabelEntryScale $w_.fwhm1 \
            -text "FWHM (major):" \
            -value $fwhm1_ \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 0.1 \
            -to 20.0 \
            -increment 1 \
            -resolution .1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_fwhm_ 1]
      }
      pack $itk_component(fwhm1) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(fwhm1) \
         {FWHM of gaussian, major axis}

      itk_component add fwhm2 {
         util::LabelEntryScale $w_.fwhm2 \
            -text "FWHM (minor):" \
            -value $fwhm2_ \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 0.1 \
            -to 20.0 \
            -increment 1 \
            -resolution .1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_fwhm_ 2]
      }
      pack $itk_component(fwhm2) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(fwhm2) \
         {FWHM of gaussian, minor axis}

      #  Prefix for name of output cube (auto-suggested until given a name).
      itk_component add prefix {
         util::LabelEntry $w_.prefix \
            -text "Output prefix:" \
            -value "GaiaTempCubeFilter" \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth)
      }
      pack $itk_component(prefix) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(prefix) \
         {Prefix for names of output cubes, will be appended by an integer}

      itk_component add outputfile {
         util::LabelValue $w_.outputfile \
            -text "Output name:" \
            -value "" \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth)
      }
      pack $itk_component(outputfile) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(outputfile) \
         {Name used for the last rebinned cube}

      #  Run the application.
      itk_component add runapp {
         button $w_.runapp -text Run \
            -command [code $this doit_]
      }
      pack $itk_component(runapp) -side top -pady 3 -padx 3
      add_short_help $itk_component(runapp) \
         {Filter the cube and load the result}

      #  Set up interface.
      toggle_type_
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Release various application tasks, if started.
      if { $blocktask_ != {} } {
         catch {$blocktask_ delete_sometime}
         set blocktask_ {}
      }
      if { $gausstask_ != {} } {
         catch {$gausstask_ delete_sometime}
         set gausstask_ {}
      }
   }

   #  Methods:
   #  --------

   #  Set the filter type.
   protected method set_filter_type_ {type} {
      set filter_type_ $type

      #  Enable/disabled appropriate elements.
      toggle_type_
   }

   #  Enable/disable components for the filter type.
   protected method toggle_type_ {} {
      switch -exact $filter_type_ {
         "square" {
            $itk_component(combination) configure -state normal
            $itk_component(boxsize1) configure -state normal
            $itk_component(boxsize1) configure -value $blocksize1_
            $itk_component(boxsize2) configure -state disabled
            $itk_component(orient) configure -state disabled
            $itk_component(fwhm1) configure -state disabled
            $itk_component(fwhm2) configure -state disabled
         }
         "rectangle" {
            $itk_component(combination) configure -state normal
            $itk_component(boxsize1) configure -state normal
            $itk_component(boxsize1) configure -value $blocksize1_
            $itk_component(boxsize2) configure -state normal
            $itk_component(boxsize2) configure -value $blocksize2_
            $itk_component(orient) configure -state disabled
            $itk_component(fwhm1) configure -state disabled
            $itk_component(fwhm2) configure -state disabled
         }
         "gaussian" {
            $itk_component(combination) configure -state disabled
            $itk_component(boxsize1) configure -state normal
            $itk_component(boxsize1) configure -value $evalsize1_
            $itk_component(boxsize2) configure -state disabled
            $itk_component(orient) configure -state disabled
            $itk_component(fwhm1) configure -state normal
            $itk_component(fwhm2) configure -state disabled
         }
         "gaussian-elliptical" {
            $itk_component(combination) configure -state disabled
            $itk_component(boxsize1) configure -state normal
            $itk_component(boxsize1) configure -value $evalsize1_
            $itk_component(boxsize2) configure -state normal
            $itk_component(boxsize2) configure -value $evalsize2_
            $itk_component(orient) configure -state normal
            $itk_component(fwhm1) configure -state normal
            $itk_component(fwhm2) configure -state normal
         }
      }
   }

   #  Set the block estimation type.
   protected method set_combination_type_ {type} {
      set combination_type_ $type
   }

   #  Set the block size.
   protected method set_boxsize_ {axis value} {
      set boxsize${axis}_ [expr max($value,1)]
      if { $filter_type_ == "square" || $filter_type_ == "rectangle" } {
         set blocksize${axis}_ [set boxsize${axis}_]
      } else {
         set evalsize${axis}_ [set boxsize${axis}_]
      }
   }

   #  Set the FWHM.
   protected method set_fwhm_ {axis value} {
      set fwhm${axis}_ [expr max($value,0.0000001)]
   }

   #  Set the orientation, must be in range 0 to 180.
   protected method set_orient_ {value} {
      set orient_ [expr min(180.0,max(0.0,$value))]
   }

   #  Run the application and display the result.
   protected method doit_ {} {

      blt::busy hold $w_

      #  Name of input cube.
      set input_name [{*}$itk_option(-gaiacube) get_ndfname]

      #  Start up the task we require.
      if { $filter_type_ == "square" || $filter_type_ == "rectangle" } {
         if { $blocktask_ == {} } {
            set blocktask_ [gaia::GaiaApp \#auto -application \
                               $::env(KAPPA_DIR)/block \
                               -notify [code $this app_completed_]]
         }
      } else {
         if { $gausstask_ == {} } {
            set gausstask_ [gaia::GaiaApp \#auto -application \
                               $::env(KAPPA_DIR)/gausmooth \
                               -notify [code $this app_completed_]]
         }
      }

      #  Record the system and units so we can restore them if needed.
      lassign [{*}$itk_option(-spec_coords) get_system] system units
      if { $system != "default" && $system != {} } {
         set keep_system_ "$system"
         set keep_units_ "$units"
      } else {
         set keep_system_ ""
         set keep_units_ ""
      }

      #  Create name for the new cube, needs to be different to the
      #  currently displayed one.
      incr count_
      set output_name_ [gaia::GaiaTempName::make_name \
                           "[$itk_component(prefix) get]" $count_ ".sdf"]
      $itk_component(outputfile) configure -value ""

      set axis [{*}$itk_option(-gaiacube) get_axis]

      switch -exact $filter_type_ {
         "square" {
            set box [get_box_ $axis $boxsize1_ $boxsize1_]
            $blocktask_ runwiths "in=$input_name out=$output_name_ \
                        box=$box estimator=$combination_type_ \
                        accept"
         }
         "rectangle" {
            set box [get_box_ $axis $boxsize1_ $boxsize2_]
            $blocktask_ runwiths "in=$input_name out=$output_name_ \
                        box=$box estimator=$combination_type_ \
                        accept"
         }
         "gaussian" {
            set axes [get_axes_ $axis]
            $gausstask_ runwiths "in=$input_name out=$output_name_ \
                        box=$boxsize1_ fwhm=$fwhm1_ orient=! \
                        axes=$axes accept"
         }
         "gaussian-elliptical" {
            set axes [get_axes_ $axis]
            $gausstask_ runwiths "in=$input_name out=$output_name_ \
                        box=\[$boxsize1_,$boxsize2_\] \
                        fwhm=\[$fwhm1_,$fwhm2_\] orient=$orient_ \
                        axes=$axes accept"
         }
      }
   }

   #  Create a BOX parameter for the given image plane sides and axis.
   protected method get_box_ {axis side1 side2} {
      if { $axis == 1 } {
         set box "\[1,$side1,$side2,1\]"
      } elseif { $axis == 2 } {
         set box "\[$side1,1,$side2\]"
      } else {
         set box "\[$side1,$side2,1\]"
      }
      return $box
   }

   #  Create an AXES parameter for the given image axis.
   protected method get_axes_ {axis} {
      if { $axis == 1 } {
         set axes "\[2,3\]"
      } elseif { $axis == 2 } {
         set axes "\[1,3\]"
      } else {
         set axes "\[1,2\]"
      }
      return $axes
   }

   #  Do the presentation of the result now the application has completed.
   protected method app_completed_ {} {

      #  Display the new cube.
      if { ! [file readable $output_name_] } {
         if { ! [file readable ${output_name_}.sdf] } {
            blt::busy release $w_
            return
         }
         set file "${output_name_}.sdf"
      } else {
         set file "$output_name_"
      }
      if { $file != {} } {
         catch {
            {*}$itk_option(-gaiacube) configure -cube "$file"
         } msg

         $itk_component(outputfile) configure -value "$file"

         #  If the file has Temp in the name, record for automatic removal.
         if { [string match "*Temp*" $file] } {
            {*}$itk_option(-gaiacube) register_temp_file $file
         }

         #  The original cube may have used a different coordinate system,
         #  switch to that if we can.
         if { $keep_system_ != {} } {
            {*}$itk_option(-spec_coords) set_system $keep_system_ $keep_units_ 0
         }
      }
      blt::busy release $w_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The related GaiaCube instance.
   itk_option define -gaiacube gaiacube GaiaCube {} {
      if { $itk_option(-gaiacube) != {} } {
         set rtdimage_ [{*}$itk_option(-gaiacube) cget -rtdimage]
      }
   }

   #  The GaiaSpecCoords instance used to define the current coordinate
   #  system.
   itk_option define -spec_coords spec_coords Spec_Coords {}

   #  Width of labels.
   itk_option define -labelwidth labelwidth LabelWidth 20

   #  Width of values.
   itk_option define -valuewidth valuewidth ValueWidth 20

   #  Protected variables: (available to instance)
   #  --------------------

   #  The BLOCK application.
   protected variable blocktask_ {}

   #  The GAUSMOOTH application.
   protected variable gausstask_ {}

   #  Type of filter.
   protected variable filter_type_ "square"

   #  Combination method.
   protected variable combination_type_ "mean"

   #  Name of the output cube.
   protected variable output_name_ {GaiaCubeFilter1.sdf}

   #  The rtdimage instance being used to display the main image.
   protected variable rtdimage_ {}

   #  Block factors/gaussian evaluation.
   protected variable boxsize1_ 3
   protected variable boxsize2_ 3

   #  Last values for block and gaussian evaluation.
   protected variable blocksize1_ 3
   protected variable blocksize2_ 3
   protected variable evalsize1_ 20
   protected variable evalsize2_ 20

   #  FWHM.
   protected variable fwhm1_ 3
   protected variable fwhm2_ 3

   #  Orientation.
   protected variable orient_ 0

   #  The system and units of the original data.
   protected variable keep_system_ {}
   protected variable keep_units_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The temporary image count.
   common count_ 0

#  End of class definition.
}

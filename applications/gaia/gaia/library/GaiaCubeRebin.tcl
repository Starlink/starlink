#+
#  Name:
#     GaiaCubeRebin

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Controls for rebinning the current cube.

#  Description:
#     This class creates a panel of controls for rebinning a cube.
#     The new cube is loaded to replace the existing one.

#  Invocations:
#
#        GaiaCubeRebin object_name [configuration options]
#
#     This creates an instance of a GaiaCubeRebin object. The return is
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
#     15-JAN-2007 (PWD):
#        Original version.
#     2012 April 20 (MJC):
#        Added support for standard of rest.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeRebin {}

itcl::class gaia::GaiaCubeRebin {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Average or sum.
      itk_component add combination {
         util::LabelMenu $w_.combination \
            -labelwidth $itk_option(-labelwidth) \
            -text "Combination method:" \
            -variable [scope combination_type_]
      }
      pack $itk_component(combination) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(combination) \
         {Method to use when combining data}

      foreach name "mean sum" {
         $itk_component(combination) add \
            -label $name \
            -value $name \
            -command [code $this set_combination_type_ $name]
      }

      #  Rebinning factors for each dimension.
      foreach n {1 2 3} {
         itk_component add bin$n {
            util::LabelEntryScale $w_.bin$n \
               -text "Bin factor, axis $n:" \
               -value [set bin${n}_] \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -from 1 \
               -to 20 \
               -increment 1 \
               -resolution 1 \
               -show_arrows 1 \
               -anchor w \
               -delay 25 \
               -command [code $this set_bin_factor_ $n]
         }
         pack $itk_component(bin$n) -side top -fill x -ipadx 1m -ipady 1m
         add_short_help $itk_component(bin$n) \
            {Binning factor for the n axis, integer 1 to size of cube}
      }

      #  Prefix for name of output cube (auto-suggested until given a name).
      itk_component add prefix {
         util::LabelEntry $w_.prefix \
            -text "Output prefix:" \
            -value "GaiaTempCubeRebin" \
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
         {Rebin the cube and load the result}
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Release various application tasks, if started.
      if { $compaddtask_ != {} } {
         catch {$compaddtask_ delete_sometime}
         set compaddtask_ {}
      }
      if { $compavetask_ != {} } {
         catch {$compavetask_ delete_sometime}
         set compavetask_ {}
      }
   }

   #  Methods:
   #  --------

   #  Set the combination type
   protected method set_combination_type_ {type} {
      set combination_type_ $type
   }

   #  Set the binning factor for an axis.
   protected method set_bin_factor_ {axis value} {
      set bin${axis}_ [expr max($value,1)]
   }

   #  Run the application and display the result.
   protected method doit_ {} {

      blt::busy hold $w_

      #  Name of input cube.
      set input_name [$itk_option(-gaiacube) get_ndfname]

      #  Start up the task we require.
      if { $combination_type_ == "mean" } {
         if { $compavetask_ == {} } {
            set compavetask_ [GaiaApp \#auto -application \
                                 $::env(KAPPA_DIR)/compave \
                                 -notify [code $this app_completed_]]
         }
         set task $compavetask_
      } else {
         if { $compaddtask_ == {} } {
            set compaddtask_ [GaiaApp \#auto -application \
                                 $::env(KAPPA_DIR)/compadd \
                                 -notify [code $this app_completed_]]
         }
         set task $compaddtask_
      }

      #  Record the system and units so we can restore them if needed.
      lassign [$itk_option(-spec_coords) get_system] system units
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

      $task runwiths "in=$input_name out=$output_name_ \
                       compress=\[$bin1_,$bin2_,$bin3_\] accept"
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
            $itk_option(-gaiacube) open_keeplimits "$file"
         } msg
         $itk_component(outputfile) configure -value "$file"

         #  If the file has Temp in the name, record for automatic removal.
         if { [string match "*Temp*" $file] } {
            $itk_option(-gaiacube) register_temp_file $file
         }

         #  The original cube may have used a different coordinate system,
         #  switch to that if we can.
         if { $keep_system_ != {} } {
            $itk_option(-spec_coords) set_system $keep_system_ $keep_units_ 0
         }
      }
      blt::busy release $w_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The related GaiaCube instance.
   itk_option define -gaiacube gaiacube GaiaCube {} {
      if { $itk_option(-gaiacube) != {} } {
         set rtdimage_ [$itk_option(-gaiacube) cget -rtdimage]
      }
   }

   #  The GaiaSpecCoords instance used to define the current coordinate
   #  system.
   itk_option define -spec_coords spec_coords Spec_Coords {}

   #  The GaiaSpecCoords instance used to define the current standard
   #  of rest.
   itk_option define -spec_sor spec_sor Spec_Coords {}

   #  Width of labels.
   itk_option define -labelwidth labelwidth LabelWidth 20

   #  Width of values.
   itk_option define -valuewidth valuewidth ValueWidth 20

   #  Protected variables: (available to instance)
   #  --------------------

   #  The COMPADD application.
   protected variable compaddtask_ {}

   #  The COMPAVE application.
   protected variable compavetask_ {}

   #  Combination method.
   protected variable combination_type_ "mean"

   #  Name of the output cube.
   protected variable output_name_ {GaiaCubeRebin1.sdf}

   #  The rtdimage instance being used to display the main image.
   protected variable rtdimage_ {}

   #  Binning factors.
   protected variable bin1_ 1
   protected variable bin2_ 1
   protected variable bin3_ 3

   #  The system and units of the original data.
   protected variable keep_system_ {}
   protected variable keep_units_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The temporary image count.
   common count_ 0

#  End of class definition.
}

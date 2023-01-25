#+
#  Name:
#     GaiaCubeApps

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Base class for applications shown in the controls panels of a GaiaCube
#     instance.

#  Description:
#     This class provides a common framework displaying and controlling
#     an application as a panel of controls in a GaiaCube instance. See
#     the concrete implementations GaiaCubeCollapse etc. for how to make
#     use of it.

#  Invocations:
#
#        GaiaCubeApps object_name [configuration options]
#
#     This creates an instance of a GaiaCubeApps object. The return is
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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     05-JUN-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeApps {}

itcl::class gaia::GaiaCubeApps {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Whether to show the various ranges as objects on the spectral plot.
      itk_component add showrange {
         gaia::StarLabelCheck $w_.showrange \
            -text "Show limits on plot:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $itk_option(-labelwidth) \
            -variable [scope itk_option(-show_ref_range)] \
            -command [code $this toggle_show_ref_range_]
      }
      pack $itk_component(showrange) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(showrange) \
         {Show reference range figure(s) on plot}

      #  Add further controls implemented by sub-class.
      add_controls_

      #  Run the application.
      itk_component add runapp {
         button $w_.collapse -text Run \
            -command [code $this doit_]
      }
      pack $itk_component(runapp) -side top -pady 3 -padx 3
      add_short_help $itk_component(runapp) \
         {Run the application and present the results}
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Release various application tasks, if started.
      if { $maintask_ != {} } {
         catch {$maintask_ delete_sometime}
         set maintask_ {}
      }

      #  Release wcsframe task.
      if { $wcsframe_ != {} } {
         catch {$wcsframe_ delete_sometime}
         set wcsframe_ {}
      }

      #  Release wcsattrib tasks.
      if { $getwcsattrib_ != {} } {
         catch {$getwcsattrib_ delete_sometime}
         set getwcsattrib_ {}
      }
      if { $setwcsattrib_ != {} } {
         catch {$setwcsattrib_ delete_sometime}
         set setwcsattrib_ {}
      }
   }

   #  Methods:
   #  --------

   #  Add extra controls between show range and Run button. Usually
   #  a range, plus combination method, so that is the default.
   protected method add_controls_ {} {
      itk_component add bounds1 {
         gaia::GaiaSpectralPlotRange $w_.bounds1 \
            -gaiacube $itk_option(-gaiacube) \
            -ref_id $itk_option(-ref_id) \
            -text1 {Lower index:} \
            -text2 {Upper index:} \
            -show_ref_range $itk_option(-show_ref_range) \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -coord_update_cmd [code $this set_limits_]
      }
      pack $itk_component(bounds1) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(bounds1) \
         {Lower and upper indices of the range}

      if { $itk_option(-show_combination) } {
         #  Method used for combination
         itk_component add combination {
            util::LabelMenu $w_.cattype \
               -labelwidth $itk_option(-labelwidth) \
               -text "Combination method:" \
               -variable [scope combination_type_]
         }
         pack $itk_component(combination) -side top -fill x -ipadx 1m -ipady 1m
         add_short_help $itk_component(combination) \
            {Method to use when combining data, use median with care}

         set top [winfo toplevel $w_]
         foreach {sname descr} $estimators_ {
            if { $descr != "" } {
               set label "$sname ($descr)"
            } else {
               set label "$sname"
            }
            $itk_component(combination) add \
               -label $label \
               -value $sname \
               -command [code $this set_combination_type_ $sname]
         }
      }
   }

   #  Set the minimum and maximum possible bounds, applies to default single
   #  range, override for different behaviour.
   public method set_bounds {plane_min plane_max} {

      set from [$itk_component(bounds1) cget -from]
      set to [$itk_component(bounds1) cget -to]
      $itk_component(bounds1) configure -from $plane_min -to $plane_max

      #  If the min/max have changed, reset the range (different axis or
      #  unrelated cube).
      if { $plane_min != $from || $plane_max != $to } {
         apply_limits $plane_min $plane_max
      } else {
         apply_limits $itk_option(-lower_limit) $itk_option(-upper_limit)
      }
   }

   #  Handle the change in the spectral reference range (user interaction by
   #  dragging or resizing range).
   public method ref_range_moved {id coord1 coord2 action} {

      #  Inhibit feedback to graphics reference range, before applying the new
      #  bounds.
      if { $action == "move" } {
         set oldvalue [$itk_component(bounds1) cget -show_ref_range]
         $itk_component(bounds1) configure -show_ref_range 0
      }

      #  Update the bounds.
      apply_limits $coord1 $coord2

      if { $action == "move" } {
         $itk_component(bounds1) configure -show_ref_range $oldvalue
      }
   }

   #  Return a list of the current limits, if they differ from the
   #  maximum and minimum bounds. Otherwise return an empty list.
   public method get_set_limits {} {
      set from [$itk_component(bounds1) cget -from]
      set to [$itk_component(bounds1) cget -to]
      if { $from != $itk_option(-lower_limit) ||
           $to != $itk_option(-upper_limit) } {
         return [list $itk_option(-lower_limit) $itk_option(-upper_limit)]
      }
      return {}
   }

   #  Apply some limits, that's set them and display the changes. Use this
   #  instead of configure -lower_limit -upper_limit, after construction.
   public method apply_limits {lower upper} {
      $itk_component(bounds1) configure -value1 $lower -value2 $upper
      set_limits_ $lower $upper
   }

   #  Set the limits of the operation (usually somewhere between the minimum
   #  and maximum bounds).
   protected method set_limits_ {bound1 bound2} {
      configure -lower_limit $bound1 -upper_limit $bound2
   }

   #  Set the combination type
   protected method set_combination_type_ {type} {
      set combination_type_ $type
   }

   #  Run the application and display the result.
   protected method doit_ {} {

      #  Convert pixel bounds to world coordinates of the cube's default
      #  system. If this fails then we need to attempt to set the current
      #  domain of the cube to PIXEL before processing.
      set lbp [expr min($itk_option(-lower_limit),$itk_option(-upper_limit))]
      set ubp [expr max($itk_option(-lower_limit),$itk_option(-upper_limit))]

      #  One twist is that the coordinate system should match that of the
      #  disk-resident cube, so check that the coordinate system hasn't been
      #  changed, if so switch back to the default system (temporarily).
      lassign [$itk_option(-spec_coords) get_system] system units
      if { $system != "default" && $system != {} } {
         $itk_option(-spec_coords) set_system "default" "default" 1
      }
      set lb [{*}$itk_option(-gaiacube) get_coord $lbp 1 0]
      set ub [{*}$itk_option(-gaiacube) get_coord $ubp 1 0]
      if { $system != "default" && $system != {} } {
         $itk_option(-spec_coords) set_system $system $units 1
      }

      set set_current_domain_ 0

      if { $lb == {} && $ub == {} } {

         #  Conversion failed, will work with PIXEL coordinates.
         set set_current_domain_ 1
         set lb [expr $lbp - 0.5]
         set ub [expr $ubp - 0.5]
         if { $wcsframe_ == {} } {
            global env
            set wcsframe_ [GaiaApp \#auto -application \
                              $env(KAPPA_DIR)/wcsframe \
                              -notify [code $this wcsframe_completed_]]
         }
         if { $getwcsattrib_ == {} } {
            global env
            set getwcsattrib_ [GaiaApp \#auto -application \
                                  $env(KAPPA_DIR)/wcsattrib \
                                  -notify [code $this wcsattrib_completed_] \
                                  -parnotify [code $this wcsattrib_gotparam_]]
         }
      }

      #  Convert cube to PIXEL domain, if needed.
      blt::busy hold $w_
      set ndfname [{*}$itk_option(-gaiacube) get_ndfname]
      if { $set_current_domain_ } {
         $getwcsattrib_ runwiths "ndf=$ndfname mode=get name=DOMAIN accept"
         ::tkwait variable [scope current_domain_]

         $wcsframe_ runwiths "ndf=$ndfname frame=PIXEL accept"
         ::tkwait variable [scope set_current_domain_]
      }

      #  Now start and run the main application, if not done already.
      set axis [{*}$itk_option(-gaiacube) get_axis]
      run_main_app_ $ndfname $axis $lb $ub

      #  If the reference lines are displayed these need removing.
      set itk_option(-show_ref_range) 0
      toggle_show_ref_range_
   }

   #  Start up the main application and run on the given ndf with the selected
   #  axis. Arrange to run the app_completed_ method as the -notify option cf.:
   #
   #    set maintask_ [GaiaApp #auto -application $env(KAPPA_DIR)/collapse \
   #                           -notify [code $this app_completed_]]
   #
   protected method run_main_app_ { ndfname axis } {
      puts "You need to implement a run_main_app_ method"
   }

   #  Do the presentation of the result now the application has completed.
   #  To handle this implement a app_present_ method.
   protected method app_completed_ {} {

      #  Get the sub-class to do the work.
      app_do_present_

      #  If we set the current domain, restore it.
      if { $set_current_domain_ } {
         set ndfname [{*}$itk_option(-gaiacube) get_ndfname]
         $wcsframe_ runwiths "ndf=$ndfname frame=$current_domain_ accept"
      }
      blt::busy release $w_
   }

   #  Method to do the presentation of the results when the main application
   #  has completed. Override to do your work (like display an image).
   protected method app_do_present_ {} {
      puts "You need to implement an app_do_present_ method"
   }

   #  Invoked when the request to obtain the current domain is completed.
   #  Note we don't use the built-in query methods for the cube
   #  (gaiautils::astget) as this may not reflect the value of the
   #  disk-resident file.
   protected method wcsattrib_completed_ {} {

      #  Get the value, but note we need to wait for this command to complete
      #  too (behaviour defined by the -parnotify option).
      $getwcsattrib_ getparam "VALUE"
      ::tkwait variable [scope parvalue_]
      set current_domain_ $parvalue_
   }
   protected method wcsattrib_gotparam_ {param val} {
      set parvalue_ $val
   }

   #  Invoked when the request to set the current domain is completed.
   protected method wcsframe_completed_ {} {
      set set_current_domain_ 1
   }

   #  Toggle the display of the reference range.
   protected method toggle_show_ref_range_ {} {
      $itk_component(bounds1) configure \
         -show_ref_range $itk_option(-show_ref_range)
      if { $itk_option(-show_ref_range) } {
         {*}$itk_option(-gaiacube) make_ref_range $itk_option(-ref_id)
         {*}$itk_option(-gaiacube) set_ref_range_colour \
            $itk_option(-ref_id) $itk_option(-ref_colour)
         $itk_component(bounds1) configure -value1 $itk_option(-lower_limit) \
            -value2 $itk_option(-upper_limit)
      } else {
         {*}$itk_option(-gaiacube) remove_ref_range $itk_option(-ref_id)
      }
   }

   #  Set the SYSTEM and UNIT values for the current coordinate system of an
   #  NDF. Use when the underlying coordinate system needs to match that in
   #  use. Note this is designed to not return until the task is complete.
   protected method set_coordinate_system_ {file system units} {
      if { $setwcsattrib_ == {} } {
         global env
         set setwcsattrib_ [GaiaApp \#auto -application \
                               $env(KAPPA_DIR)/wcsattrib \
                               -notify [code $this wcsattrib_set_]]
      }
      set wcsattrib_done_ 0
      $setwcsattrib_ runwiths "ndf=$file mode=set name=system newval=$system"
      if { ! $wcsattrib_done_ } {
         ::tkwait variable [scope wcsattrib_done_]
      }

      if { $units != {} } {
         set wcsattrib_done_ 0
         $setwcsattrib_ runwiths "ndf=$file mode=set name=unit newval=$units"
         if { ! $wcsattrib_done_ } {
            ::tkwait variable [scope wcsattrib_done_]
         }
      }
   }

   #  Called when setwcsattrib_ completes. Sets wcsattrib_done_ to trigger any
   #  tkwaits on that variable.
   protected method wcsattrib_set_ {} {
      set wcsattrib_done_ 1
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The related GaiaCube instance.
   itk_option define -gaiacube gaiacube GaiaCube {} {
      if { $itk_option(-gaiacube) != {} } {
         set canvas_ [{*}$itk_option(-gaiacube) cget -canvas]
         set rtdimage_ [{*}$itk_option(-gaiacube) cget -rtdimage]
      }
   }

   #  The spectral coordinates instance.
   itk_option define -spec_coords spec_coords Spec_Coords {}

   #  The identifier of the reference range.
   itk_option define -ref_id ref_id Ref_Id 1

   #  The colour used to display the reference range. Probably want to change
   #  this.
   itk_option define -ref_colour ref_colour Ref_Colour "cyan"

   #  Whether to show the reference range.
   itk_option define -show_ref_range show_ref_range Show_Ref_Range 0

   #  The limits of the range used by the application.
   itk_option define -lower_limit lower_limit Lower_Limit 0
   itk_option define -upper_limit upper_limit Upper_Limit 0

   #  Width of labels.
   itk_option define -labelwidth labelwidth LabelWidth 20

   #  Width of values.
   itk_option define -valuewidth valuewidth ValueWidth 20

   #  Whether to show the combination controls.
   itk_option define -show_combination show_combination Show_Combination 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Maximum and minimum possible value the limits (usually the pixel bounds
   #  of the current axis).
   protected variable plane_max_ 0
   protected variable plane_min_ 0

   #  The main application task.
   protected variable maintask_ {}

   #  The WCSFRAME task.
   protected variable wcsframe_ {}

   #  The get WCSATTRIB task.
   protected variable getwcsattrib_ {}

   #  The set WCSATTRIB task.
   protected variable setwcsattrib_ {}

   #  Combination method, if used/
   protected variable combination_type_ "mean"

   #  Name of the temporary image just created.
   protected variable tmpimage_

   #  The current domain of the cube, if used.
   protected variable current_domain_ {}

   #  Set when the current domain of cube is changed.
   protected variable set_current_domain_ 0

   #  Value of a getparam request.
   protected variable parvalue_ {}

   #  Variable for determining when wcsattrib has completed.
   protected variable wcsattrib_done_ 0

   #  The rtdimage instance being used to display the main image.
   protected variable rtdimage_ {}

   #  The canvas being used to display the main image.
   protected variable canvas_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  All the known collapse estimators for COLLAPSE and CHANMAP, short and
   #  long descriptions.
   common estimators_ {
      Mean ""
      WMean {Weighted Mean}
      Mode ""
      Median ""
      Absdev {Mean absolute deviation}
      CMean {Clipped mean}
      Comax {Co-ordinate of the maximum value}
      Comin {Co-ordinate of the minimum value}
      CSigma {Clipped standard deviation}
      Integ {Integrated value}
      Iwc {Intensity-weighted co-ordinate}
      Iwd {Intensity-weighted dispersion}
      Max Maximum
      Min Minimum
      Rms ""
      Sigma {Standard deviation}
      Sum ""
   }

   #  The temporary image count.
   common count_ 0

#  End of class definition.
}

#+
#  Name:
#     GaiaCubeBaseline

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Controls for the subtraction of a base-line from a data cube using
#     MFITTREND.

#  Description:
#     This class creates a panel of controls for defining the ranges
#     to use when fitting and subtracting trends to each spectral line
#     of a data cube. See the KAPPA MFITTREND application.

#  Invocations:
#
#        GaiaCubeBaseline object_name [configuration options]
#
#     This creates an instance of a GaiaCubeBaseline object. The return is
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
#     gaia::GaiaCubeApps

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
#     31-MAY-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeBaseline {}

itcl::class gaia::GaiaCubeBaseline {

   #  Inheritances:
   #  -------------
   inherit gaia::GaiaCubeApps

   #  Constructor:
   #  ------------
   constructor {args} {
      eval gaia::GaiaCubeApps::constructor $args -ref_colour "green" \
         -show_combination 0
   } {
      #  Evaluate any options [incr Tk].
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      #  Nothing to do.
   }

   #  Methods:
   #  --------
   protected method init {} {

      #  We use a number of ranges, also add a control for enabling/disabling
      #  the range.
      for {set i 0} {$i < $itk_option(-nranges)} {incr i} {

         set enable_range_($i) 0
         itk_component add enable$i {
            gaia::StarLabelCheck $childsite_.enable$i \
               -text "Enable:" \
               -onvalue 1 -offvalue 0 \
               -labelwidth $itk_option(-labelwidth) \
               -variable [scope enable_range_($i)] \
               -command [code $this toggle_enable_ranges_]
         }
         pack $itk_component(enable$i) -side top -fill x -ipadx 1m -ipady 1m
         add_short_help $itk_component(enable$i) {Enable this range}

         itk_component add bounds$i {
            gaia::GaiaSpectralPlotRange $childsite_.bounds$i \
               -gaiacube $itk_option(-gaiacube) \
               -ref_id [expr $itk_option(-ref_id) + $i]\
               -text1 {Lower index:} \
               -text2 {Upper index:} \
               -show_ref_range $itk_option(-show_ref_range) \
               -labelwidth $itk_option(-labelwidth) \
               -valuewidth $itk_option(-valuewidth) \
               -coord_update_cmd [code $this set_limits_ $i]
         }
         pack $itk_component(bounds$i) -side top -fill x -ipadx 1m -ipady 1m
         add_short_help $itk_component(bounds$i) \
            {Lower and upper indices of the range}
      }
   }

   #  Called from GaiaCubeApps constructor to add UI controls. Need to
   #  reserve space (and do real work in init) if using local configuration
   #  options.
   protected method add_controls_ {} {

      #  No default controls.
      itk_component add order {
         util::LabelEntryScale $w_.order \
            -text "Order of fits:" \
            -value 1 \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 0 \
            -to 15 \
            -increment 1 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 100 \
            -command [code $this set_order_]
      }
      pack $itk_component(order) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(order) \
         {Order of polynomials to use in fits}

      #  Prefix for name of output cube (auto-suggested until given a name).
      itk_component add prefix {
         util::LabelEntry $w_.prefix \
            -text "Output prefix:" \
            -value "GaiaTempCube" \
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
         {Name used for the last baselined cube}

      #  Put the main controls in a scrollframe to manage the real estate
      #  usage.
      itk_component add scrollframe {
         ::iwidgets::scrolledframe $w_.scrollframe \
            -height 200 -hscrollmode none -vscrollmode dynamic
      }
      pack $itk_component(scrollframe) -fill both -expand 1
      set childsite_ [$itk_component(scrollframe) childsite]
   }

   #  Set one of the limits.
   protected method set_limits_ {n bound1 bound2} {
      set lower_limits_($n) $bound1
      set upper_limits_($n) $bound2
   }

   public method set_bounds {plane_min plane_max} {

      if { [info exists lower_limits_] } {
         set from [$itk_component(bounds0) cget -from]
         set to [$itk_component(bounds0) cget -to]
      } else {
         set from -1
         set to -1
      }

      #  If the min/max have changed, also reset the ranges (different axis or
      #  unrelated cube).
      if { $plane_min != $from || $plane_max != $to } {
         for {set i 0} {$i < $itk_option(-nranges)} {incr i} {
            $itk_component(bounds$i) configure -from $plane_min -to $plane_max
            apply_limits $i $plane_min $plane_max
         }
      } else {
         #  Just set the limits.
         for {set i 0} {$i < $itk_option(-nranges)} {incr i} {
            apply_limits $i $lower_limits_($i) $upper_limits_($i)
         }
      }
   }

   #  Return a list of the current limits for the given bound, if they differ
   #  from the maximum and minimum bounds. Otherwise return an empty list.
   public method get_set_limits {n} {
      set from [$itk_component(bounds$n) cget -from]
      set to [$itk_component(bounds$n) cget -to]
      if { $from != $lower_limits_($n) || $to != $upper_limits_($n) } {
         return [list $lower_limits_($n) $upper_limits_($n)]
      }
      return {}
   }

   #  Apply some limits, that's set them and display the changes.
   public method apply_limits {n lower upper} {
      $itk_component(bounds$n) configure -value1 $lower -value2 $upper
      set_limits_ $n $lower $upper
   }

   #  Run MFITTREND to do the work, and replace the existing cube with the new
   #  one.
   protected method run_main_app_ { ndfname axis lb ub} {

      #  Now startup the MFITTREND application.
      if { $maintask_ == {} } {
         global env
         set maintask_ [GaiaApp \#auto -application \
                            $env(KAPPA_DIR)/mfittrend \
                            -notify [code $this app_completed_]]
      }

      #  Create a temporary file name.
      incr count_
      set tmpimage_ [gaia::GaiaTempName::make_name \
                        "[$itk_component(prefix) get]" $count_ ".sdf"]
      $itk_component(outputfile) configure -value ""

      #  Need to determine ranges. Note handle case when coordinate system
      #  doesn't match the disk-file.
      lassign [$itk_option(-spec_coords) get_system] system units
      if { $system != "default" && $system != {} } {
         set keep_system_ "$system"
         set keep_units_ "$units"
         $itk_option(-spec_coords) set_system "default" "default" 1
      } else {
         set keep_system_ {}
         set keep_units_ {}
      }
      set ranges ""
      for {set i 0} {$i < $itk_option(-nranges)} {incr i} {
         if { $enable_range_($i) } {
            set lbp [expr min($lower_limits_($i),$upper_limits_($i))]
            set ubp [expr max($lower_limits_($i),$upper_limits_($i))]
            set lb [$itk_option(-gaiacube) get_coord $lbp 1 0]
            set ub [$itk_option(-gaiacube) get_coord $ubp 1 0]
            lappend ranges $lb $ub
         }
      }
      if { $system != "default" && $system != {} } {
         $itk_option(-spec_coords) set_system $system $units 1
      }

      if { $ranges != {} } {
         set ranges [join $ranges ","]
         $maintask_ runwiths "in=$ndfname out=$tmpimage_ axis=$axis \
                           order=$itk_option(-order) subtract=true \
                           modifyin=false ranges=\"$ranges\" accept"
      } else {
         warning_dialog "There are no ranges enabled"
         catch {blt::busy release $w_}
      }
   }

   #  Open and display the new cube.
   protected method app_do_present_ {} {
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
         $itk_option(-gaiacube) configure -cube $file

         #  The original cube may have used a different coordinate system,
         #  switch to that if we can.
         if { $keep_system_ != {} } {
            $itk_option(-spec_coords) set_system $keep_system_ $keep_units_ 0
         }

         #  Show name of results.
         $itk_component(outputfile) configure -value "$file"
      }
      blt::busy release $w_
   }

   #  Set a new order value.
   protected method set_order_ {value} {
      configure -order $value
   }

   #  Handle the change in one of the spectral reference range (user
   #  interaction by dragging or resizing range).
   public method ref_range_moved {id coord1 coord2 action} {

      set n [expr $id - $itk_option(-ref_id)]

      #  Inhibit feedback to graphics reference range, before applying the new
      #  bounds.
      if { $action == "move" } {
         set oldvalue [$itk_component(bounds$n) cget -show_ref_range]
         $itk_component(bounds$n) configure -show_ref_range 0
      }

      #  Update the bounds.
      apply_limits $n $coord1 $coord2

      if { $action == "move" } {
         $itk_component(bounds$n) configure -show_ref_range $oldvalue
      }
   }

   #  Toggle the display of the reference ranges.
   protected method toggle_show_ref_range_ {} {
      for {set i 0} {$i < $itk_option(-nranges)} {incr i} {

         $itk_component(bounds$i) configure \
            -show_ref_range $itk_option(-show_ref_range)
         set id [expr $itk_option(-ref_id) + $i]

         if { $enable_range_($i) } {
            $itk_option(-gaiacube) make_ref_range $id
            $itk_option(-gaiacube) set_ref_range_colour $id \
               $itk_option(-ref_colour)
            $itk_component(bounds$i) configure \
               -value1 $lower_limits_($i) \
               -value2 $upper_limits_($i)
         } else {
            $itk_option(-gaiacube) remove_ref_range $id
         }
      }
   }

   #  Handle a change in the enabled state of a range.
   protected method toggle_enable_ranges_ {} {
      toggle_show_ref_range_
  }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Order of the polynomials.
   itk_option define -order order Order 1

   #  Number of ranges.
   itk_option define -nranges nranges Nranges 4

   #  Protected variables: (available to instance)
   #  --------------------

   #  The limits of the ranges.
   protected variable lower_limits_
   protected variable upper_limits_

   #  Which ranges are enabled.
   protected variable enable_range_

   #  The childsite of scrolledframe.
   protected variable childsite_ {}

   #  The system and units of the original data.
   protected variable keep_system_ {}
   protected variable keep_units_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

#+
#  Name:
#     GaiaCubeCollapse

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Controls for the collapse of a cube displayed by a GaiaCube.

#  Description:
#     This class creates a panel of controls for collapsing a range of planes
#     of a cube into a "white-light" image. The image is then displayed in the
#     main GAIA window.

#  Invocations:
#
#        GaiaCubeCollapse object_name [configuration options]
#
#     This creates an instance of a GaiaCubeCollapse object. The return is
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     31-MAY-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeCollapse {}

itcl::class gaia::GaiaCubeCollapse {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Whether to show the collapse limits as a range object on the
      #  spectral plot.
      itk_component add showrange {
         StarLabelCheck $w_.showrange \
            -text "Show limits on plot:" \
            -onvalue 1 -offvalue 0 \
            -labelwidth $itk_option(-labelwidth) \
            -variable [scope itk_option(-show_ref_range)] \
            -command [code $this toggle_show_ref_range_]
      }
      pack $itk_component(showrange) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(showrange) \
         {Show extent of collapse on plot with a reference range figure}

      itk_component add bounds {
         GaiaSpectralPlotRange $w_.bounds \
            -gaiacube $itk_option(-gaiacube) \
            -ref_id $itk_option(-ref_id) \
            -text1 {Lower index:} \
            -text2 {Upper index:} \
            -show_ref_range $itk_option(-show_ref_range) \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -coord_update_cmd [code $this set_collapse_bounds_]
      }
      pack $itk_component(bounds) -side top -fill x -ipadx 1m -ipady 2m
      add_short_help $itk_component(bounds) \
         {Lower and upper indices used for creating collapsed image}

      #  Method used for collapse.
      itk_component add combination {
         LabelMenu $w_.cattype \
            -labelwidth $itk_option(-labelwidth) \
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
         button $w_.collapse -text Collapse \
            -command [code $this collapse_]
      }
      pack $itk_component(collapse) -side top -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(collapse) \
         {Display the combined image collapsed along the range}

   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Release collapser task.
      if { $collapser_ != {} } {
         catch {$collapser_ delete_sometime}
         set collapser_ {}
      }
   }

   #  Methods:
   #  --------

   #  Set the minimum and maximum possible bounds.
   public method set_bounds {plane_min plane_max} {
      $itk_component(bounds) configure -from $plane_min -to $plane_max
      $itk_component(bounds) configure -value1 $plane_min -value2 $plane_max
      set_collapse_bounds_ $plane_min $plane_max
   }

   #  Handle the change in the spectral reference range (user interaction by
   #  dragging or resizing range).
   public method ref_range_moved {coord1 coord2 action} {

      #  Inhibit feedback to graphics reference range, before applying the new
      #  bounds. 
      if { $action == "move" } {
         set oldvalue [$itk_component(bounds) cget -show_ref_range]
         $itk_component(bounds) configure -show_ref_range 0
      }
      
      #  Update the bounds.
      $itk_component(bounds) configure -value1 $coord1 -value2 $coord2
      set_collapse_bounds_ $coord1 $coord2

      if { $action == "move" } {
         $itk_component(bounds) configure -show_ref_range $oldvalue
      }
   }

   #  Set the collapse bounds.
   protected method set_collapse_bounds_ {bound1 bound2} {
      configure -lower_bound $bound1 -upper_bound $bound2
   }

   # Set the combination type
   protected method set_combination_type_ {type} {
      set combination_type_ $type
   }
   
   #  Collapse image and the display the result.
   #  Use a section to pass to COLLAPSE so we do not need to know the world
   #  coordinates.
   protected method collapse_ {} {
      set lb [expr min($itk_option(-lower_bound),$itk_option(-upper_bound))]
      set ub [expr max($itk_option(-lower_bound),$itk_option(-upper_bound))]
      set range "$lb:$ub"
      set axis [$itk_option(-gaiacube) get_axis]
      if { $axis == 1 } {
         set section "($range,,$itk_option(-close_section)"
      } elseif { $axis == 2 } {
         set section "(,$range,$itk_option(-close_section)"
      } else {
         set section "(,,${range}${itk_option(-close_section)}"
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
      set ndfname [$itk_option(-gaiacube) get_ndfname]
      $collapser_ runwiths "in=${ndfname}$section out=$tmpimage_ axis=$axis \
                            estimator=$combination_type_ accept"

      #  If the reference lines are displayed these need removing.
      set itk_option(-show_ref_range) 0
      toggle_show_ref_range_
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
         $itk_option(-gaiacube) display $file 1
      }
      blt::busy release $w_
   }

   #  Toggle the display of the collapse reference range.
   protected method toggle_show_ref_range_ {} {
      $itk_component(bounds) configure \
         -show_ref_range $itk_option(-show_ref_range)
      if { $itk_option(-show_ref_range) } {
         $itk_option(-gaiacube) make_ref_range $itk_option(-ref_id)
         $itk_option(-gaiacube) set_ref_range_colour \
            $itk_option(-ref_id) "cyan"
         $itk_component(bounds) configure -value1 $itk_option(-lower_bound) \
            -value2 $itk_option(-upper_bound)
      } else {
         $itk_option(-gaiacube) remove_ref_range $itk_option(-ref_id)
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The related GaiaCube instance.
   itk_option define -gaiacube gaiacube GaiaCube {}
   
   #  The identifier of the reference range.
   itk_option define -ref_id ref_id Ref_Id 2

   #  Whether to show the reference range.
   itk_option define -show_ref_range show_ref_range Show_Ref_Range 0

   #  Collapse bounds.
   itk_option define -lower_bound lower_bound Lower_Bound 0
   itk_option define -upper_bound upper_bound Upper_Bound 0

   #  Width of labels.
   itk_option define -labelwidth labelwidth LabelWidth 20

   #  Width of values.
   itk_option define -valuewidth valuewidth ValueWidth 20

   #  The terminator characters for closing a section. May specify
   #  a final redundant axis, should be updated with the GaiaCube instance.
   itk_option define -close_section close_section Close_Section ")"

   #  Protected variables: (available to instance)
   #  --------------------

   #  Maximum and minimum possible value for plane.
   protected variable plane_max_ 0
   protected variable plane_min_ 0

   #  The COLLAPSE task.
   protected variable collapser_ {}

   #  Combination method.
   protected variable combination_type_ "Mean"

   #  Name of the temporary image just created.
   protected variable tmpimage_

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
      Sum Sum
   }

   #  The temporary image count.
   common count_ 0

#  End of class definition.
}

#+
#  Name:
#     GaiaCubeAnimation

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Controls for the animation of a cube displayed by a GaiaCube.

#  Description:
#     This class creates a panel of controls for animating through
#     a sequence of planes of a cube being handled by a related GaiaCube
#     instance.

#  Invocations:
#
#        GaiaCubeAnimation object_name [configuration options]
#
#     This creates an instance of a GaiaCubeAnimation object. The return is
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
#     30-MAY-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaCubeAnimation {}

itcl::class gaia::GaiaCubeAnimation {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options [incr Tk].
      eval itk_initialize $args

      #  Whether to show the animation limits as a range object on the
      #  spectral plot.
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
         {Show extent of animation on plot with a reference range figure}

      itk_component add bounds {
         gaia::GaiaSpectralPlotRange $w_.bounds \
            -gaiacube $itk_option(-gaiacube) \
            -ref_id $itk_option(-ref_id) \
            -text1 {Lower index:} \
            -text2 {Upper index:} \
            -value1 $plane_ \
            -value2 $plane_ \
            -show_ref_range $itk_option(-show_ref_range) \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -coord_update_cmd [code $this set_limits_]
      }
      pack $itk_component(bounds) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(bounds) \
         {Lower and upper indices used during animation}

      #  Delay used in animation.
      itk_component add delay {
         util::LabelEntryScale $w_.delay \
            -text {Delay (milli):} \
            -value $itk_option(-delay) \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 10 \
            -to 1000 \
            -increment 10 \
            -resolution 10 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_delay_]
      }
      pack $itk_component(delay) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(delay) \
         {Delay used during animation in milliseconds}

      #  Step between planes.
      itk_component add step {
         util::LabelEntryScale $w_.step \
            -text {Step:} \
            -value $itk_option(-step) \
            -labelwidth $itk_option(-labelwidth) \
            -valuewidth $itk_option(-valuewidth) \
            -from 1 \
            -to 100 \
            -increment 2 \
            -resolution 1 \
            -show_arrows 1 \
            -anchor w \
            -delay 25 \
            -command [code $this set_step_]
      }
      pack $itk_component(step) -side top -fill x -ipadx 1m -ipady 1m
      add_short_help $itk_component(step) \
         {Step between frames of animation}

      #  Looping behaviour.
      itk_component add loopframe {
         frame $w_.frame
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

      pack $itk_component(loopframe) -side top -fill x -ipadx 1m -ipady 1m

      #  Capture to animated GIF, exclusive with looping so shares
      #  same radiobutton state.
      itk_component add captureframe {
         frame $w_.capture
      }
      itk_component add capturelabel {
         label $itk_component(captureframe).label \
            -text "Capture:" \
            -width 21 \
            -anchor w
      }
      pack $itk_component(capturelabel) -side left -fill none

      itk_component add captureon {
         radiobutton $itk_component(captureframe).captureon \
            -text "On" \
            -variable [scope loop_] \
            -value "capture"
      }
      pack $itk_component(captureon) -side left -fill none
      add_short_help $itk_component(captureon) \
         {Capture to an animated gif (GaiaAnimation.gif)}

      pack $itk_component(captureframe) -side top -fill x -ipadx 1m -ipady 1m

      #  Animation stop and start.
      itk_component add animation {
         frame $w_.animation
      }
      pack $itk_component(animation) -side top -fill x -ipadx 1m -ipady 1m

      itk_component add stop {
         button $itk_component(animation).stop -text Stop \
            -command [code $this stop]
      }
      pack $itk_component(stop) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(stop) {Stop animation}

      itk_component add start {
         button $itk_component(animation).start -text Start \
            -command [code $this start_]
      }
      pack $itk_component(start) -side right -expand 1 -pady 3 -padx 3
      add_short_help $itk_component(start) {Start animation}
   }

   #  Destructor:
   #  -----------
   destructor  {

      #  Stop animation.
      stop
   }

   #  Methods:
   #  --------

   #  Set the minimum and maximum possible bounds of the animation.
   public method set_bounds {plane_min plane_max} {
      set from [$itk_component(bounds) cget -from]
      set to [$itk_component(bounds) cget -to]
      $itk_component(bounds) configure -from $plane_min -to $plane_max

      #  If the min/max have changed, reset the animation range (different
      #  axis or unrelated cube).
      if { $plane_min != $from || $plane_max != $to } {
         apply_limits $plane_min $plane_max
      } else {
         apply_limits $itk_option(-lower_limit) $itk_option(-upper_limit)
      }
   }

   #  Return a list of the current animation limits, if they differ from the
   #  maximum and minimum bounds. Otherwise return an empty list.
   public method get_set_limits {} {
      set from [$itk_component(bounds) cget -from]
      set to [$itk_component(bounds) cget -to]
      if { $from != $itk_option(-lower_limit) ||
           $to != $itk_option(-upper_limit) } {
         return [list $itk_option(-lower_limit) $itk_option(-upper_limit)]
      }
      return {}
   }

   #  Apply an animation range, that's set it and display the changes.
   #  Use this instead of configure -lower_limit -upper_limit, after
   #  construction.
   public method apply_limits {lower upper} {
      $itk_component(bounds) configure -value1 $lower -value2 $upper
      set_limits_ $lower $upper
   }

   #  Set the animation limits. Same as configure lower_limit & upper_limit.
   protected method set_limits_ {lower upper} {
      configure -lower_limit $lower -upper_limit $upper
   }

   #  Handle the change in the spectral reference range (user interaction by
   #  dragging or resizing range).
   public method ref_range_moved {id coord1 coord2 action} {

      #  Inhibit feedback to graphics reference range, before applying the new
      #  bounds.
      if { $action == "move" } {
         set oldvalue [$itk_component(bounds) cget -show_ref_range]
         $itk_component(bounds) configure -show_ref_range 0
      }

      #  Update the bounds.
      apply_limits $coord1 $coord2

      if { $action == "move" } {
         $itk_component(bounds) configure -show_ref_range $oldvalue
      }
   }

   #  Start the animation.
   protected method start_ {} {
      set capframes_ {}
      set initial_seconds_ [clock clicks -milliseconds]
      if { $afterId_ == {} } {
         if { $itk_option(-lower_limit) > $itk_option(-upper_limit) } {
            set step_ [expr -1*$itk_option(-step)]
         } else {
            set step_ $itk_option(-step)
         }
         set plane_ $itk_option(-lower_limit)
         {*}$itk_option(-gaiacube) set_display_plane $itk_option(-lower_limit) 0
         increment_
      }
   }
   protected variable initial_seconds_ 0

   #  Stop the animation, if running.
   public method stop {} {
      if { $afterId_ != {} } {
         after cancel $afterId_
         set afterId_ {}
         # DEBUG
         puts "animated for: [expr [clock clicks -milliseconds] - $initial_seconds_]"

         #  Update the WCS so that the spectral axis coordinate is correct.
         {*}$itk_option(-gaiacube) update_wcs

         #  Create the animated GIF, if required.
         if { $loop_ == "capture" && $capframes_ != {} } {
            create_animated_gif_
         }
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

   #  Check if plane_ is inside the limits. Handles lower being higher
   #  than upper, which just means run in the different direction.
   protected method inside_ {} {
      if { $itk_option(-lower_limit) > $itk_option(-upper_limit) } {
         if { $plane_ > $itk_option(-upper_limit) &&
              $plane_ <= $itk_option(-lower_limit) } {
            return 1
         }
      } else {
         if { $plane_ >= $itk_option(-lower_limit) &&
              $plane_ < $itk_option(-upper_limit) } {
            return 1
         }
      }
      return 0
   }

   #  Increment the displayed section by one.
   protected method increment_ {} {
      if { [inside_] } {
         set plane_ [expr ${plane_}+$step_]
         {*}$itk_option(-gaiacube) set_display_plane $plane_ 0

         if { $loop_ == "capture" } {
            if { ! [capture_] } {
               #  Didn't work. Tidy up and stop.
               set nfrm [llength $capframes_]
               for { set i 0 } { $i < $nfrm } { incr i } {
                  ::file delete -force [lindex $capframes_ $i]
               }
               set capframes_ {}
               stop
               return
            }
         }
         if { $plane_ == $itk_option(-lower_limit) } {
            #  At lower edge, running backwards, need to let it step below.
            set plane_ [expr ${plane_}+$step_]
         }
         set afterId_ [after $itk_option(-delay) [code $this increment_]]
      } else {
         #  Off end so stop, or loop back to beginning, or go into reverse
         #  with rock 'n roll option.
         #  Check that we have a range, otherwise this will call increment_
         #  causing an eval depth exception. We also stop when capturing.
         if { $itk_option(-lower_limit) == $itk_option(-upper_limit) ||
              $loop_ == "capture" } {
            stop
            return
         } else {
            #  Force temporary halt as visual clue that end has arrived.
            update idletasks
            after 500
            if { $loop_ != "off" } {
               if { $loop_ != "on" } {
                  #  Rock 'n roll, switch direction.
                  if { $itk_option(-lower_limit) >
                       $itk_option(-upper_limit) } {
                     if { $step_ >= 1 } {
                        # Going down.
                        set plane_ $itk_option(-lower_limit)
                     } else {
                        # Going up.
                        set plane_ [expr $itk_option(-upper_limit) + 1]
                     }
                  } else {
                     if { $step_ >= 1 } {
                        # Going up.
                        set plane_ $itk_option(-lower_limit)
                     } else {
                        # Going down.
                        set plane_ [expr $itk_option(-upper_limit) - 1]
                     }
                  }
                  set step_ [expr -1*$step_]
               } else {
                  #  Standard loop.
                  if { $itk_option(-lower_limit) >
                       $itk_option(-upper_limit) } {
                     set step_ [expr -1*abs($step_)]
                  } else {
                     set step_ [expr abs($step_)]
                  }
                  set plane_ $itk_option(-lower_limit)
               }
               increment_
            } else {
               stop
               return
            }
         }
      }
   }

   #  Reduce the number of colours in the given image to 256 and write to the
   #  given filename in GIF. Would like to use Tk extension but nothing seems
   #  to work (like blt::winop quantize), so offer this facility if the "djpeg"
   #  program is available.
   protected method reduce_colours_ {image filename} {
      if { [auto_execok djpeg] != {} } {
         set tmpfile \
            [gaia::GaiaTempName::make_name GaiaTempImage [pid] ".jpeg"]

         #  Save as JPEG with no losses and convert to GIF.
         $image write $tmpfile -format "jpeg -quality 100"
         ::exec djpeg -gif -colors 256 -outfile $filename $tmpfile
         ::file delete -force $tmpfile
      } else {
         info_dialog "Failed to save snapshot: too many colors \
                      and no djpeg command available"
         return 0
      }
      return 1
   }

   #  Capture the current image to a GIF.
   protected method capture_ {} {

      #  Do snapshot.
      ::update idletasks
      ::after $itk_option(-delay)
      set canvas [{*}$itk_option(-gaiacube) cget -canvas]
      set image [::image create photo -format window -data $canvas]
      incr capcount_
      set gif [gaia::GaiaTempName::make_name \
                  "GaiaTempAnimation" $capcount_ ".gif"]
      lappend capframes_ $gif

      #  This can fail if their aretoo many colors. In that case attempt
      #  to reduce the number of colours before giving up.
      if { [catch {$image write $gif -format gif} msg ] } {
         if { $msg == "too many colors" } {
            if { [reduce_colours_ $image $gif] == 0 } {
               ::image delete $image
               return 0
            }
         }
      }
      ::image delete $image
      return 1
   }

   #  Create an animated GIF from the captured frames.
   protected method create_animated_gif_ {} {
      set nfrm [llength $capframes_]
      if { $nfrm > 1 } {
         busy {

            ::gifblock::gif.load gif(0) [lindex $capframes_ 0]

            for {set i 1} {$i < $nfrm} {incr i} {
               ::gifblock::gif.load gif($i) [lindex $capframes_ $i]

               ::gifblock::gif.createGC gif(0) \
                  [expr int($itk_option(-delay)/10)]

               set data [::gifblock::gif.get gif($i) {Color Table} 0]
               ::gifblock::gif.add gif(0) $data

               set data [::gifblock::gif.get gif($i) {Image Descriptor} 0]
               ::gifblock::gif.add gif(0) $data

               set gif($i) {}
               ::file delete -force [lindex $capframes_ $i]
            }
            ::gifblock::gif.save gif(0) "GaiaAnimation.gif"
            set gif(0) {}
            ::file delete -force [lindex $capframes_ 0]
         }
      }
      set capframes_ {}
   }

   #  Toggle the display of the animation reference range.
   protected method toggle_show_ref_range_ {} {
      $itk_component(bounds) configure \
         -show_ref_range $itk_option(-show_ref_range)
      if { $itk_option(-show_ref_range) } {
         {*}$itk_option(-gaiacube) make_ref_range $itk_option(-ref_id)
         {*}$itk_option(-gaiacube) set_ref_range_colour \
            $itk_option(-ref_id) "yellow"
         $itk_component(bounds) configure -value1 $itk_option(-lower_limit) \
            -value2 $itk_option(-upper_limit)
      } else {
         {*}$itk_option(-gaiacube) remove_ref_range $itk_option(-ref_id)
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The related GaiaCube instance.
   itk_option define -gaiacube gaiacube GaiaCube {}

   #  The animation delay (ms).
   itk_option define -delay delay Delay 100

   #  The animation step defined in interface.
   itk_option define -step step Step 1 {
      set step_ $itk_option(-step)
   }

   #  The identifier of the reference range.
   itk_option define -ref_id ref_id Ref_Id 1

   #  Whether to show the reference range.
   itk_option define -show_ref_range show_ref_range Show_Ref_Range 0

   #  Animation bounds.
   itk_option define -lower_limit lower_limit Lower_Limit 0
   itk_option define -upper_limit upper_limit Upper_Limit 0

   #  Width of labels.
   itk_option define -labelwidth labelwidth LabelWidth 20

   #  Width of values.
   itk_option define -valuewidth valuewidth ValueWidth 20

   #  Protected variables: (available to instance)
   #  --------------------

   #  The current plane along the current axis.
   protected variable plane_ 1

   #  Maximum and minimum possible value for plane.
   protected variable plane_max_ 0
   protected variable plane_min_ 0

   #  Id of the animation thread.
   protected variable afterId_ {}

   #  How animation loops. Off by default.
   protected variable loop_ "off"

   #  The current value of step during an animation.
   protected variable step_ 1

   #  Index for captured frames.
   protected variable capframes_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Count of captured GIFs.
   common capcount_ 0

#  End of class definition.
}

#+
#  Name:
#     StarBlink

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class for blinking all known image clones.

#  Description:
#     This class provides methods for creating and controlling
#     the "blinking" of all the currently displayed images.
#
#     All images are displayed and can cycled through by hand
#     or by a timed animated. One of the displayed images is
#     designated as mobile and can be moved using the keyboard
#     arrows. The display range, scale etc. are determined by the
#     primary images which are being viewed.
#
#     If desired all the displayed images may be combined together
#     into another image and displayed in a clone. This uses the
#     CCDPACK application MAKEMOS.

#  Invocations:
#
#        StarBlink object_name [configuration options]
#
#     This creates an instance of a StarBlink object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#        -width width {512}
#
#    Defines the width of the part of screen used to display the images.
#
#        -height height {512}
#
#    Defines the height of the part of screen used to display the images.
#
#        -millisec milliseconds {25}
#
#    Defines the base delay used for animations. All intervals are
#    a multiple of this number.
#
#        -time time {200}
#
#    The animation delay.

#  Methods:
#
#        animate_on
#
#     This activates the animation of the images.
#
#        raise_next index
#
#     Raises the image with index to the top of the stack making
#     it visible and then activates an animation of the next image.
#
#        view_next
#
#     Raises the "next" in the display cycle to the top. No animation
#     is produced.
#
#        animate_off
#
#     Stops the current animation if running.
#
#        close
#
#     Closes the window and stops any animations.
#
#        change_speed factor
#
#     Determines the wait period between animations as factor times the
#     current millisecond setting.
#
#        set_mobile
#
#     Sets the mobile image to the one currently on top of the stack.

#  Inheritance:
#     This widget inherits TopLevelWidget.

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
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
#     25-NOV-1996 (PWD):
#        Original version.
#     20-JAN-1996 (PWD):
#        Added scrollbars to control view, clone number display and
#        offset views.
#     20-MAR-1997 (PWD):
#        Added button to refresh images. Tried doing this on every
#        move of the image, but this is far too slow for interactive
#        scrolling.
#     20-MAR-1998 (PWD):
#        Added facilities for combining images.
#     {enter_further_changes_here}

#-

#.

itk::usual StarBlink {}

itcl::class gaia::StarBlink {

   #  Inheritances:
   #  -------------

   inherit TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args

      #  Close down of window needs to stop animation.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]
      wm title $w_ "GAIA: Blink displayed images ($itk_option(-number))"

      #  Add menu bar for help.
      add_menubar
      global gaia_dir
      add_help_button $gaia_dir/StarBlink.hlp "On Window..."

      #  Frame for buttons.
      itk_component add Bframe {
	  frame $w_.bframe
      }

      #  Add a button to close the window.
      itk_component add Close {
	  button $itk_component(Bframe).close \
		  -text {Close} -command [code $this close]
      }

      #  Add a button to refresh the images (the view offsets are wrong
      #  when the images are moved on the canvas, so you gradually lose
      #  bits, this redraws the images from the data, rather than just
      #  moving the pixmap about).
      itk_component add Refresh {
	  button $itk_component(Bframe).refresh \
		  -text {Refresh} -command [code $this refresh_images_]
      }

      #  Add a button start the animation.
      itk_component add Blinkon {
	  button $itk_component(Bframe).on \
		  -text {Blink on} -command [code $this animate_on]
      }

      #  Add a button start the animation.
      itk_component add Blinkoff {
	  button $itk_component(Bframe).off \
		  -text {Blink off} -command [code $this animate_off]
      }

      #  Slider for speeding and slowing the animation.
      itk_component add Sframe {
	  frame $w_.sframe
      }
      itk_component add Speed {
	  LabelNumber $itk_component(Sframe).speed \
		  -text {Speed factor:} \
		  -command [code $this change_speed] \
		  -value 8 \
                  -valuewidth 3 \
                  -min 1 \
                  -max 100
      }

      #  Button to request the view of the next image.
      itk_component add Next {
	  button $itk_component(Sframe).next \
		  -text {View next image} \
		  -command [code $this view_next]
      }

      #  Button for setting the mobile image.
      itk_component add Mobile {
	  button $itk_component(Sframe).mobile \
		  -text {Mark image mobile} \
		  -command [code $this set_mobile]
      }

      #  Frame for image info.
      itk_component add Iframe {
         frame $w_.iframe
      }

      #  Label for identifying the current clone.
      itk_component add Name {
         LabelMessage $itk_component(Iframe).name \
            -text {Clone:} -value {}
      }

      #  Offset of clone's lower corner.
      itk_component add Xlow {
         util::LabelEntry $itk_component(Iframe).xlow \
            -text {X offset:} -command [code $this place_image_ x] \
            -value 1
      }
      itk_component add Ylow {
         util::LabelEntry $itk_component(Iframe).ylow \
            -text {Y offset:} -command [code $this place_image_ y] \
            -value 1
      }

      #  Create a frame to hold all views and scrollbars (views are
      #  placed one above each other).
      itk_component add MainFrame {
	  frame $w_.frame -border 3
      } {
	  keep -width -height
      }
      itk_component add Frame {
	  frame $itk_component(MainFrame).frame -border 3
      } {
	  keep -width -height
      }

      #  Add scrollbars to move the view about.
      itk_component add Vscroll {
         scrollbar $itk_component(MainFrame).vscroll \
            -relief sunken \
            -command [code $this vscroll]
      }
      itk_component add Hscroll {
         scrollbar $itk_component(MainFrame).hscroll \
            -relief sunken \
            -orient horiz \
            -command [code $this hscroll]
      }

      #  Add button to combine and display the images.
      itk_component add Combine {
         button $w_.combine \
            -text {Combine} \
            -command [code $this combine]
      }

      #  Pack all widgets into place.
      pack $itk_component(Bframe) -side bottom -fill x  -ipadx 5 -ipady 5
      pack $itk_component(Close) -side right -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Refresh) -side right -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Blinkon) -side right -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Blinkoff) -side right -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Sframe) -side bottom -fill x -ipadx 5 -ipady 5
      pack $itk_component(Speed) -side left -expand 1 -padx 5 -pady 5
      pack $itk_component(Mobile) -side left -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Next) -side left -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Iframe) -side bottom -fill x -ipadx 5 -ipady 5
      pack $itk_component(Name) -fill x -side left -ipadx 3 -ipady 3
      pack $itk_component(Xlow) -side top -ipadx 3
      pack $itk_component(Ylow) -side bottom -ipadx 3
      pack $itk_component(MainFrame) -side top -expand 1 -fill both
      pack $itk_component(Hscroll) -side bottom -fill x
      pack $itk_component(Vscroll) -side right -fill y
      pack $itk_component(Combine) -side bottom -fill x
      pack $itk_component(Frame) -side left -expand 1 -fill both

      #  And add all the views.
      add_views_
   }

   #  Destructor:
   #  -----------
   destructor {
      if { $makemos_ != {} } {
         catch {$makemos_ delete_sometime}
         set makemos_ {}
      }
   }

   #  Methods:
   #  --------

   #  Close down and stop animations.
   method close {} {
      animate_off

      #  Remove all views setup by this class (these are deferred by
      #  RTD until complete application shutdown otherwise).
      if { $n_ > 0 } {
         for { set i 0 } { $i < $n_ } { incr i } {
            catch {
               set target [$clones_($i) get_image]
               set view [$itk_component(image$i) get_image]
               $target view remove $view
            }
         }
      }
      destroy $w_
   }

   #  Locate and add all the known views in main windows.
   #  These are all called rtdnnnn, starting at 0 and working
   #  up in number.
   private method add_views_ {} {
      set clones "[uplevel #0 info command .rtd?] \
                  [uplevel #0 info command .rtd??] \
                  [uplevel #0 info command .rtd???] \
                  [uplevel #0 info command .rtd????]"
      set n_ 0
      foreach w $clones {
         itk_component add image$n_ {
            RtdImage $itk_component(Frame).image$n_ -graphics 0 \
               -scrollbars 0
         }
         set canvas_($n_) [$itk_component(image$n_) get_canvas]
         set image_($n_) [$itk_component(image$n_) get_image]
         set target_($n_) [$w get_image]

         regsub {\.rtd} [winfo toplevel $w] {} clone_num_($n_)
         set clones_($n_) $w
         set canvas $canvas_($n_)

         #  Stop the cursor from warping in canvas and add bindings
         #  to move the mobile image.
         bind $canvas <Left>  [code $this move_mobile_ -1 0]
         bind $canvas <Right> [code $this move_mobile_ 1 0]
         bind $canvas <Up>    [code $this move_mobile_ 0 -1]
         bind $canvas <Down>  [code $this move_mobile_ 0 1]
         bind $canvas <Control-Left>  [code $this move_mobile_ -5 0]
         bind $canvas <Control-Right> [code $this move_mobile_ 5 0]
         bind $canvas <Control-Up>    [code $this move_mobile_ 0 -5]
         bind $canvas <Control-Down>  [code $this move_mobile_ 0 5]
         bind $canvas <Shift-Left>  [code $this move_mobile_ -25 0]
         bind $canvas <Shift-Right> [code $this move_mobile_ 25 0]
         bind $canvas <Shift-Up>    [code $this move_mobile_ 0 -25]
         bind $canvas <Shift-Down>  [code $this move_mobile_ 0 25]

	 #  Make <Return> cycle throw the images.
         bind $canvas <Return>  [code $this view_next]

         #  Set canvas movements to change scrollbars.
         $canvas configure -xscrollcommand "$itk_component(Hscroll) set"
         $canvas configure -yscrollcommand "$itk_component(Vscroll) set"

         #  Set the view and place the image (use place to overlay).
         $target_($n_) view add $image_($n_)
         place $itk_component(image$n_)  -x 0 -y 0 -anchor nw \
            -relwidth 1.0 -relheight 1.0
         $itk_component(image$n_) center
         incr n_
      }
      set top_ [expr $n_ -1]
      set_mobile

      #  Finally describe the current image in info section.
      update
      top_clone_
   }

   #  Move the views up the display stack.
   method animate_on {} {
      eval set id_ \
         [after $itk_option(-time) [code catch "$this raise_next" dummy]]
   }

   #  See the next image and set animation.
   method raise_next {} {
      incr top_
      if { $top_ >= $n_ } { set top_ 0 }
      update
      raise $itk_component(image$top_)
      animate_on
   }

   #  Plain view of next image.
   method view_next {} {
      incr top_
      if { $top_ >= $n_ } { set top_ 0 }
      raise $itk_component(image$top_)
      top_clone_
   }

   #  Stop animations.
   method animate_off {} {
      if { $id_ != {} } {
         after cancel $id_
         set id_ {}
         update
      }
      top_clone_
   }

   #  Change the speed of animation.
   method change_speed {factor} {
      configure -time [expr $factor*$itk_option(-millisec)]
   }

   #  Set the mobile image.
   method set_mobile {} {
      set mobile_ $top_
   }

   #  Scroll images vertically.
   method vscroll {args} {
      if { $n_ > 0 } {
         for { set i 0 } { $i < $n_ } { incr i } {
            eval $canvas_($i) yview $args

            #  Make sure image scale changes are seen now.
            set w [$image_($i) dispwidth]
            set h [$image_($i) dispheight]
            $canvas_($i) configure -scrollregion "0 0 $w $h"
         }
      }
   }

   #  Scroll images horizontally.
   method hscroll {args} {
      if { $n_ > 0 } {
         for { set i 0 } { $i < $n_ } { incr i } {
            eval $canvas_($i) xview $args

            #  Make sure image scale changes are seen now.
            set w [$image_($i) dispwidth]
            set h [$image_($i) dispheight]
            $canvas_($i) configure -scrollregion "0 0 $w $h"
         }
      }
   }

   #  Set the top clone information.
   private method top_clone_ {} {

      #  Clone number.
      $itk_component(Name) configure -value $clone_num_($top_)

      #  Image offset.
      set imageId [$itk_component(image$top_) get_imageId]
      lassign [$canvas_($top_) bbox $imageId] x0 y0 x1 y1
      $image_($top_) convert coords $x0 $y1 canvas x0 y1 image
      $itk_component(Xlow) configure -value $x0
      $itk_component(Ylow) configure -value $y1
   }

   #  Shift the current image to a new position.
   private method place_image_ {dir new} {
      set tag [$itk_component(image$top_) get_imageId]
      lassign [$canvas_($top_) bbox $tag] x0 y0 x1 y1
      $image_($top_) convert coords $x0 $y1 canvas x0 y1 image
      if { $dir == "x" } {
         set dx [expr $new-$x0]
         $image_($top_) convert dist $dx 0 image dx dummy canvas
         $canvas_($top_) move $tag $dx 0
      } else {
         set dy [expr $y1-$new]
         $image_($top_) convert dist 0 $dy image dummy dy canvas
         $canvas_($top_) move $tag 0 $dy
      }
      update_view_ $top_
   }

   #  Move the mobile image to a new position.
   private method move_mobile_ {dx dy} {
      raise $itk_component(image$mobile_)
      set top_ $mobile_
      top_clone_
      set tag [$itk_component(image$mobile_) get_imageId]
      $canvas_($mobile_) move $tag $dx $dy

#  Experimental Move image using placer code.
#       set x [expr [winfo x $itk_component(image$mobile_)] + ($dx)]
#       set y [expr [winfo y $itk_component(image$mobile_)] + ($dy)]
#       place $itk_component(image$mobile_) -x $x -y $y \
#          -bordermode outside -relwidth 1.0 -relheight 1.0
   }

   #  Update the offsets of the view when activity drops (do not do this all
   #  the time as this is very slow).
   private method update_view_ { n } {
      set tag [$itk_component(image$n) get_imageId]
      lassign [$canvas_($n) bbox $tag] x0 y0 x1 y1
      set w [expr $x1-$x0+1]
      set h [expr $y1-$y0+1]
      $target_($n) view update $image_($n) 0 0 $w $h $x0 $y0 0 0 canvas
   }

   #  Refresh all the views. The view offsets are wrong when the images
   #  are moved on the canvas, so you gradually lose bits, this redraws
   #  the images from the data, rather than just
   #  moving the pixmap about).
   private method refresh_images_ {}  {
      if { $n_ > 0 } {
         for { set i 0 } { $i < $n_ } { incr i } {
            update_view_ $i
         }
      }
   }

   #  Use the current offsets to combine, normalise and display the
   #  images.
   public method combine {} {

      #  Get a list of all the displayed files and their offsets (as
      #  NDF sections).
      if { $n_ > 0 } {

         #  Startup KAPPA:SLIDE if needed.
         if { $slide_ == {} } {
            global env
            set slide_ [GaiaApp \#auto -application \
                           $env(KAPPA_DIR)/slide \
                           -notify [code $this slide_complete_]]
         }
         blt::busy hold $w_
         set namelist [::open "StarBlink.names" w]
         set tempfiles_ {}
         for { set i 0 } { $i < $n_ } { incr i } {
            set oldfilename [$target_($i) cget -file]
            lassign [$canvas_($i) bbox $image_($i)] x0 y0 x1 y1
            $image_($i) convert coords $x0 $y1 canvas xoff yoff image

            #  Resample all images to new offsets.
            set oldfilename [file rootname $oldfilename]
            set newfilename "${oldfilename}_[pid]"

            #  Assume all files are base NDFs for now...
            puts $namelist "$newfilename"
            lappend tempfiles_ $newfilename

            #  Shift them to reflect the origin change.
            $slide_ runwith "inpic=$oldfilename" "stype=absolute" \
               "absx=$xoff" "absy=$yoff" "outpic=$newfilename"
         }
         ::close $namelist
      }
   }

   #  Method to detect the end of all the image sliding.
   private method slide_complete_ {} {
      if { [$slide_ status] == "waiting..." } {
         #  Proceed with combining images. Startup MAKEMOS if needed.
         if { $makemos_ == {} } {
            global env
            set makemos_ [GaiaApp \#auto -application \
                             $env(CCDPACK_DIR)/makemos \
                             -notify [code $this finished_combine_]]
         }
         $makemos_ runwith in=^StarBlink.names scale zero out=StarBlink
      }
   }

   #  Method to detect image combination completed. When so image is
   #  displayed in a new clone, unless we've already got one to work
   #  with.
   private method finished_combine_ {} {

      #  Remove all the temporary files.
      eval file delete $tempfiles_

      set gotcha 0
      if { $myclone_ != {} } {
         if { [winfo exists $myclone_] } {
            set gotcha 1
         }
      }
      if { $gotcha } {
         $myclone_ open StarBlink.sdf
      } else {
         set clones "
                     [uplevel #0 info command .rtd?] \
                     [uplevel #0 info command .rtd??] \
                     [uplevel #0 info command .rtd???] \
                     [uplevel #0 info command .rtd????]"
         set baseclone [lindex $clones 0]
         set myclone_ [$baseclone clone {} StarBlink.sdf]
      }
      blt::busy release $w_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Width and height of image display area.
   itk_option define -width width Width {512}
   itk_option define -height height Height {512}

   #  Number of milliseconds to use when calculating time factors.
   itk_option define -millisec millisec MilliSec {25}

   #  Time between animations.
   itk_option define -time time Time {200}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Set the name of the application and find the associated
   #  monolith, if one exists, if not pretend application is a
   #  monolith .

   #  Protected variables: (available to instance)
   #  --------------------
   protected variable n_ 0
   protected variable id_ {}
   protected variable top_ -1
   protected variable mobile_ 0
   protected variable clones_
   protected variable clone_num_
   protected variable canvas_
   protected variable image_
   protected variable target_
   protected variable update_pending_ 0

   protected variable myclone_ {}
   protected variable makemos_ {}
   protected variable slide_ {}
   protected variable tempfiles_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}

#+
#  Name:
#     GaiaBlink

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class for blinking all known image clones.

#  Description:
#     This class provides methods for creating and controlling
#     the "blinking" of all the currently displayed images.
#
#     A selection of the all images are displayed and can cycled
#     through by hand or by a timed animated. One of the displayed
#     images is designated as mobile and can be moved using the keyboard
#     arrows. The display range, scale etc. are determined by the primary
#     images which are being viewed.

#  Invocations:
#
#        GaiaBlink object_name [configuration options]
#
#     This creates an instance of a GaiaBlink object. The return is
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
#     See itk_options definitions.

#  Methods:
#     See method declarations.

#  Inheritance:
#     This widget inherits TopLevelWidget.

#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
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
#     ALLAN: Allan Brighton (ESO)
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
#     06-MAY-1998 (ALLAN):
#        Use SkyCat::get_skycat_images to get clones, use "cget -number" to get
#        clone number.
#     12-NOV-1998 (PWD):
#        Added ability to select amongst images and added ScrollArrows
#        for obvious orientation changes.
#     10-JUL-2003 (PWD):
#        Finally renamed GaiaBlink from StarBlink. Now uses a single
#        canvas with many rtdimages, provides alignment by origin
#        information.
#     07-OCT-2003 (PWD):
#        Alignment by WCS origins.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaBlink {}

itcl::class gaia::GaiaBlink {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {
      eval itk_initialize $args

      #  Close down of window needs to stop animation.
      wm protocol $w_ WM_DELETE_WINDOW [code $this close]
      wm title $w_ "GAIA: Blink compare images ($itk_option(-number))"

      #  Create menubar and add File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      add_short_help $itk_component(menubar).file {File menu:}

      #  Add short_help window.
      make_short_help

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Add option to list the offsets for all images.
      add_menuitem $File command "Save offsets" \
         {Save all offsets to file GaiaOffsets.Log} \
         -command [code $this save_offsets_]

      #  Set the exit item.
      $File add command -label {Close window} \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]
      $short_help_win_ add_menu_short_help $File \
         {Close window} {Close toolbox}

      #  Add option to control some actions that may be removed when
      #  the controls panel is hidden or are not provided there.
      set Actions [add_menubutton "Actions"]
      configure_menubutton Actions -underline 0
      add_short_help $itk_component(menubar).actions \
         {Choose an action}

      add_menuitem $Actions command "Position like main image" \
         {Position scroll so that image (1) is visible as displayed} \
         -command [code $this position_like_image_ 0]

      add_menuitem $Actions command "Apply grid positions" \
         {Align images at lower left corner} \
         -command [code $this fix_origins_]

      add_menuitem $Actions command "Apply WCS shifts" \
         {Attempt to position images using WCS projected shifts} \
         -command [code $this set_wcs_origins_]

      add_menuitem $Actions command "Apply NDF origins" \
         {Attempt to position images using any NDF origin information} \
         -command [code $this set_ndf_origins_]

      add_menuitem $Actions command "Apply FITS CRPIX origins" \
         {Attempt to position images using FITS CRPIX1 and CRPIX2 values} \
         -command [code $this set_fits_origins_]

      add_menuitem $Actions command "Blink on" \
         {Start blinking images} \
         -command [code $this animate_on]

      add_menuitem $Actions command "Blink off" \
         {Stop blinking images} \
         -command [code $this animate_off]

      add_menuitem $Actions command "Speed up" \
         {Decrease time between blinks} \
         -command [code $this delta_change_speed 0.9]

      add_menuitem $Actions command "Speed down" \
         {Increase time between blinks} \
         -command [code $this delta_change_speed 1.1]

      #  Add option to control which images are displayed. Use a
      #  menu of checkbuttons.
      set Images [add_menubutton "Images"]
      configure_menubutton Images -underline 0
      add_short_help $itk_component(menubar).images \
        {Select images to display}

      #  Add option to control the appearance of the backbox. This
      #  provides a classic look when raised just before the current image.
      set Options [add_menubutton "Options"]
      configure_menubutton Options -underline 0
      add_short_help $itk_component(menubar).options \
         {Choose additional options}

      $Options add checkbutton \
         -label {Classic view} \
         -variable [scope itk_option(-classic_view)] \
         -onvalue 1 \
         -offvalue 0
      $short_help_win_ add_menu_short_help $Options \
         {Classic view} \
         {Switch to a view with one image showing}

      add_menuitem $Options cascade "Background" \
         {Choose a colour for the background} \
         -menu [menu $Options.colour]
      foreach i $itk_option(-colors) {
         $Options.colour add radiobutton \
            -value $i \
            -background $i \
            -variable [scope itk_option(-background_colour)] \
            -command [code $this set_background_ $i]
      }

      $Options add separator
      add_menuitem $Options checkbutton "Hide Control Area" \
         {Toggle the visibility of the controls area} \
         -variable [scope itk_option(-hide_controls)] \
         -onvalue 1 -offvalue 0 \
         -command [code $this hide_controls_]

      #  Add help menu.
      add_help_button blink "On Window..."

      #  Frame for all controls.
      itk_component add ControlsFrame {
         frame $w_.cframe
      }

      #  Frame for buttons.
      itk_component add Bframe {
         frame $itk_component(ControlsFrame).bframe
      }

      #  Add a button to close the window.
      itk_component add Close {
         button $itk_component(Bframe).close \
            -text {Close} -command [code $this close]
      }
      add_short_help $itk_component(Close) \
         {Close window}

      #  Add a button start the animation.
      itk_component add Blinkon {
         button $itk_component(Bframe).on \
            -text {Blink on} -command [code $this animate_on]
      }
      add_short_help $itk_component(Blinkon) \
         {Start blinking by cycling through selected images}

      #  Add a button stop the animation.
      itk_component add Blinkoff {
         button $itk_component(Bframe).off \
            -text {Blink off} -command [code $this animate_off]
      }
      add_short_help $itk_component(Blinkoff) {Stop blinking}

      #  Slider for speeding and slowing the animation.
      itk_component add Sframe {
         frame $itk_component(ControlsFrame).sframe
      }
      itk_component add Speed {
         util::LabelNumber $itk_component(Sframe).speed \
            -text {Speed factor:} \
            -command [code $this change_speed] \
            -value 24 \
            -valuewidth 3 \
            -min 1 \
            -max 100
      }
      add_short_help $itk_component(Speed) \
         {Relative speed of blink, less for faster}

      #  Button to request the view of the next image.
      itk_component add Next {
         button $itk_component(Sframe).next \
            -text {View next image} \
            -command [code $this view_next]
      }
      add_short_help $itk_component(Next) \
         {Display next selected image}

      #  Button for setting the mobile image.
      itk_component add Mobile {
         button $itk_component(Sframe).mobile \
            -text {Mark image mobile} \
            -command [code $this set_mobile]
      }
      add_short_help $itk_component(Mobile) \
         {Make the current image the mobile one, that is one that moves}

      #  Frame for image info.
      itk_component add Iframe {
         frame $itk_component(ControlsFrame).iframe
      }

      #  Label for identifying the current clone.
      itk_component add Name {
         util::LabelValue $itk_component(Iframe).name \
            -text {Image:} -value {} -valuewidth 30 -justify right
      }
      add_short_help $itk_component(Blinkon) \
         {Name of current image}

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
      add_short_help $itk_component(Xlow) \
         {Offset from mobile image, or offset to apply (<return>)}
      add_short_help $itk_component(Ylow) \
         {Offset from mobile image, or offset to apply (<return>)}

      #  Add arrows for adjusting image position.
      itk_component add Arrows {
         gaia::ScrollArrows $itk_component(Iframe).arrows \
            -command [code $this move_mobile_]
      }
      add_short_help $itk_component(Arrows) \
         {Press to scroll mobile image position}

      #  Create a frame to hold the main canvas and scrollbars.
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

      #  Pack all widgets into place.
      pack $itk_component(ControlsFrame) -side bottom -fill x -ipadx 5 -ipady 5
      pack $itk_component(Bframe) -side bottom -fill x  -ipadx 5 -ipady 5
      pack $itk_component(Close) -side right -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Blinkon) -side right -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Blinkoff) -side right -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Sframe) -side bottom -fill x -ipadx 5 -ipady 5
      pack $itk_component(Speed) -side left -expand 1 -padx 5 -pady 5
      pack $itk_component(Mobile) -side left -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Next) -side left -expand 1 -ipadx 3 -ipady 3
      pack $itk_component(Iframe) -side bottom -fill x -ipadx 5 -ipady 5
      pack $itk_component(Name) -fill x -side left -ipadx 3 -ipady 3
      pack $itk_component(Arrows) -side left -ipadx 3
      pack $itk_component(Xlow) -side top -ipadx 3
      pack $itk_component(Ylow) -side bottom -ipadx 3
      pack $itk_component(MainFrame) -side top -expand 1 -fill both
      pack $itk_component(Hscroll) -side bottom -fill x
      pack $itk_component(Vscroll) -side right -fill y
      pack $itk_component(Frame) -side left -expand 1 -fill both

      #  Add all the images to the canvas.
      add_views_ $Images
   }

   #  Destructor:
   #  -----------
   destructor {
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Close down and stop animations.
   public method close {} {
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

   #  Locate and add all the known views in main windows.  These are
   #  returned by the get_skycat_images command. Also populate the
   #  Images menu with checkbuttons to control which images are
   #  displayed.
   private method add_views_ {menu} {

      set cmd \
         [list canvas $itk_component(Frame).canvas \
             -xscrollincr 1 \
             -yscrollincr 1 \
             -background black \
             -insertofftime 0 \
             -confine true ]

      itk_component add canvas {
         set canvas_ [uplevel "#0" $cmd]
      } {
         rename -relief -canvasrelief canvasRelief CanvasRelief
         rename -borderwidth -canvasborderwidth canvasBorderwidth CanvasBorderwidth
         rename -background -canvasbackground canvasBackground CanvasBackground
         rename -width -canvaswidth canvasWidth CanvasWidth
         rename -height -canvasheight canvasHeight CanvasHeight
      }
      place $canvas_  -x 0 -y 0 -anchor nw -relwidth 1.0 -relheight 1.0

      #  Stop the cursor from warping in canvas and add bindings
      #  to move the mobile image.
      bind $canvas_ <Left>  [code $this move_mobile_ -1 0]
      bind $canvas_ <Right> [code $this move_mobile_ 1 0]
      bind $canvas_ <Up>    [code $this move_mobile_ 0 -1]
      bind $canvas_ <Down>  [code $this move_mobile_ 0 1]
      bind $canvas_ <Control-Left>  [code $this move_mobile_ -5 0]
      bind $canvas_ <Control-Right> [code $this move_mobile_ 5 0]
      bind $canvas_ <Control-Up>    [code $this move_mobile_ 0 -5]
      bind $canvas_ <Control-Down>  [code $this move_mobile_ 0 5]
      bind $canvas_ <Shift-Left>  [code $this move_mobile_ -25 0]
      bind $canvas_ <Shift-Right> [code $this move_mobile_ 25 0]
      bind $canvas_ <Shift-Up>    [code $this move_mobile_ 0 -25]
      bind $canvas_ <Shift-Down>  [code $this move_mobile_ 0 25]

      #  Make <Return> cycle through the images.
      bind $canvas_ <Return>  [code $this view_next]

      #  A "c" positions like the main image.
      bind $w_ <KeyPress-c> [code $this position_like_image_ 0]

      #  <1> in canvas area gives it focus
      bind $canvas_ <1> [code focus $canvas_]

      #  Set canvas movements to change scrollbars.
      $canvas_ configure -xscrollcommand "$itk_component(Hscroll) set"
      $canvas_ configure -yscrollcommand "$itk_component(Vscroll) set"

      #  Convert window positions into canvas positions.
      set canvasX_ "\[$canvas_ canvasx %x\]"
      set canvasY_ "\[$canvas_ canvasx %y\]"

      set n_ 0
      foreach w [skycat::SkyCat::get_skycat_images] {
         set cmd [list image create rtdimage]
         set cmd \
            [list image create rtdimage \
                -name "blink image $n_" \
                -displaymode 1 \
                -usexshm $itk_option(-usexshm) \
                -usexsync $itk_option(-usexsync) \
                -verbose $itk_option(-verbose) \
                -subsample $itk_option(-subsample)]

         set image_($n_) [uplevel "#0" $cmd]
         set target_($n_) [$w get_image]
         set names_($n_) [$w cget -file]

         #  Set the image to view the parent image.
         $target_($n_) view add $image_($n_)

         #  Put the image in the canvas.
         $canvas_ create image 0 0  \
            -image $image_($n_) -anchor nw -tags "$image_($n_) images"

         set clone_num_($n_) [[winfo toplevel $w] cget -number]
         set clones_($n_) $w

         #  Select and move the image.
         $canvas_ bind $image_($n_) <1> \
            [code eval $this mark_image_ $n_ $canvasX_ $canvasY_]
         $canvas_ bind $image_($n_) <B1-Motion> \
            [code eval $this move_image_ $n_ $canvasX_ $canvasY_]
         $canvas_ bind $image_($n_) <ButtonRelease-1> \
            [code eval $this update_image_ $n_]

         $menu add checkbutton \
            -label "$names_($n_)($clone_num_($n_))" \
            -variable [scope view_($n_)] \
            -onvalue 1 \
            -offvalue 0
         set view_($n_) 1

         incr n_
      }
      set top_ [expr $n_ -1]
      set_mobile $top_
      update

      #  Create the backbox. Needed to hide other images.
      set backbox_ [$canvas_ create rect 0 0 1 1 \
                       -fill $itk_option(-background_colour)]
      $canvas_ configure -background $itk_option(-background_colour)
      $canvas_ lower $backbox_

      #  Modify the scrollbar positions to reflect the image position
      #  of the first clone.
      set_origins_
      init_scroll_
   }

   #  Move the views up the display stack.
   public method animate_on {} {
      if { $id_ == {} } {
         set stop_now_ 0
         raise_next_
      }
   }

   #  See the next image and set next raise event. If this image isn't
   #  to be viewed pass straight on to next.
   protected method raise_next_ {} {

      #  When stop_now_ is set a pending stop request is in action,
      #  don't compromise this by starting a new raise_next_ before the
      #  stop has had a chance to complete.
      if { $stop_now_ } {
         set id_ {}
         return
      }

      incr top_
      if { $top_ >= $n_ } { set top_ 0 }
      if { $view_($top_) } {
         if { $itk_option(-classic_view) } {
            $canvas_ raise $backbox_
         } else {
            $canvas_ lower $backbox_
         }
         $canvas_ raise $image_($top_)
         update
         eval set id_ \
            [after $itk_option(-time) [code catch "$this raise_next_" dummy]]
      } else {
         eval set id_ \
            [after 0 [code catch "$this raise_next_" dummy]]
      }
   }

   #  Plain view of next image.
   public method view_next {} {
      incr top_
      if { $top_ >= $n_ } {
         set top_ 0
      }
      if { ! $view_($top_) } {
         #  Look for next image that can be displayed.
         for { set i 0} { $i < $n_ } { incr i; incr top_ } {
            if { $top_ >= $n_ } {
               set top_ 0
            }
            if { $view_($top_) } {
               break
            }
         }
      }
      if { $itk_option(-classic_view) } {
         $canvas_ raise $backbox_
      } else {
         $canvas_ lower $backbox_
      }
      $canvas_ raise $image_($top_)
      top_clone_
   }

   #  Stop animations.
   public method animate_off {} {
      if { $id_ != {} } {
         set stop_now_ 1
         after cancel $id_
         after cancel [code catch "$this raise_next_" dummy]
         set id_ {}
         update
      }
      top_clone_
   }

   #  Change the speed of animation by a relative amount.
   public method delta_change_speed {delta} {
      configure -time [expr int($itk_option(-time)*$delta)]
   }

   #  Change the speed of animation.
   public method change_speed {factor} {
      configure -time [expr $factor*$itk_option(-millisec)]
   }

   #  Record a mark for the position of an image and make it the top clone.
   public method mark_image_ { n x y } {
      set xmark_ $x
      set ymark_ $y
      set_mobile $n
   }

   #  Move an image to a new canvas position relative to the marked position.
   protected method move_image_ {index x y} {
      $canvas_ raise $image_($index)
      $canvas_ move $image_($index) [expr $x-$xmark_] [expr $y-$ymark_]
      set xmark_ $x
      set ymark_ $y
   }

   #  Update after an image been moved (by dragging).
   protected method update_image_ {n} {
      set_scroll_region_
      set_mobile $n
   }

   #  Set the mobile image. Also becomes the top image if a value is
   #  given.
   public method set_mobile { {n {}} } {
      if { $n == {} } {
         set mobile_ $top_
      } else {
         set mobile_ $n
         $canvas_ raise $image_($n)
         top_clone_ $n
      }
   }

   #  Scroll images vertically.
   public method vscroll {args} {
      eval $canvas_ yview $args
   }

   #  Scroll images horizontally.
   public method hscroll {args} {
      eval $canvas_ xview $args
   }

   #  Get the region occupied by an image on the canvas.
   protected method get_image_area_ { n originc } {
      $image_($n) convert coords 1 1 image x0 y0 canvas

      $image_($n) convert coords \
         [expr [$image_($n) width]-1] [expr [$image_($n) height]-1] image \
         x1 y1 canvas

      set cx0 [expr int(min($x0,$x1))]
      set cy0 [expr int(min($y0,$y1))]
      set cx1 [expr int(max($x0,$x1))]
      set cy1 [expr int(max($y0,$y1))]

      #  Correct for canvas origin.
      if { $originc } {
         lassign [$canvas_ coords $image_($n)] ccx0 ccy0
         set cx0 [expr int($cx0+$ccx0)]
         set cy0 [expr int($cy0+$ccy0)]
         set cx1 [expr int($cx1+$ccx0)]
         set cy1 [expr int($cy1+$ccy0)]
      }
      return "$cx0 $cy0 $cx1 $cy1"
   }

   #  Set the top clone information. If n is set then this become the
   #  top_ image.
   private method top_clone_ { {n -1} } {
      if { $n != -1 } {
         set top_ $n
      }
      #  Clone filename and number.
      $itk_component(Name) configure \
         -value "$names_($top_) ($clone_num_($top_))"

      #  Image offsets WRT to mobile image.
      lassign [get_offsets_ $top_] x0 y0

      $itk_component(Xlow) configure -value $x0
      $itk_component(Ylow) configure -value $y0
   }

   #  Return the offsets of an image WRT the mobile image. Image
   #  coordinates.
   protected method get_offsets_ { n } {
      lassign [get_image_area_ $n 1] tx0 ty0 tx1 ty1
      lassign [get_image_area_ $mobile_ 1] mx0 my0 mx1 my1

      $image_($n) convert dist \
         [expr $tx0-$mx0+1] [expr $my1-$ty1+1] canvas x0 y0 image

      return "$x0 $y0"
   }

   #  Shift the current image to a new position.
   protected method place_image_ {dir new} {
      place_animage_ $top_ $dir $new
   }

   #  Shift an image to a new position. This is an offset from the
   #  mobile image in image coordinates.
   private method place_animage_ {index dir new} {

      #  Get origin of mobile image on canvas.
      lassign [$canvas_ coords $image_($mobile_)] mx0 my0

      #  And current position of image.
      lassign [get_image_area_ $index 1] x0 y0 x1 y1

      if { $dir == "x" } {

         $image_($mobile_) convert coords $new 0 image cx cy canvas
         set dx [expr $cx+$mx0]
         $canvas_ coords $image_($index) $dx $y0

      } else {

         # Position at upper left for canvas coords.
         set yup [expr $new + [$image_($index) height]-1]

         $image_($mobile_) convert coords 1 $yup image cx cy canvas
         set dy [expr $cy+$my0]
         $canvas_ coords $image_($index) $x0 $dy

      }
      update_view_ $index

   }

   #  Move the mobile image to a new position.
   private method move_mobile_ {dx dy} {
      $canvas_ raise $image_($mobile_)
      set top_ $mobile_
      top_clone_
      $canvas_ move $image_($mobile_) $dx $dy
      set_scroll_region_
   }

   #  Update the offsets of the view when activity drops (do not do this all
   #  the time as this is very slow).
   private method update_view_ { n } {
      lassign [$canvas_ bbox $image_($n)] x0 y0 x1 y1
      set w [expr $x1-$x0+1]
      set h [expr $y1-$y0+1]
      $target_($n) view update $image_($n) 0 0 $w $h $x0 $y0 0 0 canvas
   }

   #  Refresh all the views. The view offsets are wrong when the images
   #  are moved on the canvas, so you gradually lose bits, this redraws
   #  the images from the data, rather than just moving the pixmap about).
   private method refresh_images_ {}  {
      if { $n_ > 0 } {
         for { set i 0 } { $i < $n_ } { incr i } {
            update_view_ $i
         }
      }
   }

   #  Set the scrollregion big enough to encompass all images.
   protected method set_scroll_region_ {} {
      if { $n_ > 0 } {
         refresh_images_
         lassign [$canvas_ bbox "images"] minx miny maxx maxy
         $canvas_ configure -scrollregion "$minx $miny $maxx $maxy"
         $canvas_ coords $backbox_ $minx $miny $maxx $maxy
      }
   }

   #  Modify the scroll to reflect the relative positioning
   #  of the first clone (so we can see what it shows).
   protected method init_scroll_ {} {
      if { [info exists clones_(0)] } {
         set_scroll_region_
         lassign [$canvas_ cget -scrollregion] x0 y0 x1 y1
         $target_(0) convert coords 0 0 screen xo yo image
         $image_(0) convert coords $xo $yo image xleft yleft canvas
         if { $xleft > 0.0 } {
            hscroll moveto [expr ($xleft-$x0)/($x1-$x0)]
         }
         if { $yleft > 0.0 } {
            vscroll moveto [expr ($yleft-$y0)/($y1-$y0)]
         }
         set_scroll_region_
      }
   }

   #  Modify the scroll to reflect the positioning the given clones parent
   #  display (so we roughly can see what it shows).
   protected method position_like_image_ {n} {
      if { [info exists clones_($n)] } {
         #set_scroll_region_
         set canvas [$clones_($n) get_canvas]
         lassign [$canvas xview] x0 x1
         lassign [$canvas yview] y0 y1
         hscroll moveto $x0
         vscroll moveto $y0
      }
   }

   #  Set the canvas positions of the images to reflect any image
   #  origins available. All images are first positioned at 0 0,
   #  i.e. the upper left of canvas (this is already true at start,
   #  but is needed for later calls to this set of methods).
   protected method set_origins_ {} {
      #  Use method for the type of data of mobile image.
      if { [$image_($mobile_) isfits] } {
         set_fits_origins_
      } else {
         set_ndf_origins_
      }
   }

   protected method set_ndf_origins_ {} {
      catch {
         if { [info exists clones_(0)] } {

            #  Place mobile image at 0 0 and get image coordinates of
            #  upper left.
            $canvas_ coords $image_($mobile_) 0 0
            $image_($mobile_) origin bxo byo
            set byo [expr $byo+[$image_($mobile_) height]-1]

            set_scroll_region_
            for { set i 0 } { $i < $n_ } { incr i } {

               #  Origin of upper left of this image (image coordinates).
               $image_($i) origin xo yo
               set yo [expr $yo+[$image_($i) height]-1]

               # Pixel shift from mobile image upper left to this one.
               set dx [expr $xo-$bxo]
               set dy [expr $byo-$yo]

               # Equivalent canvas shift.
               $image_($mobile_) convert dist $dx $dy image dx dy canvas

               #  Apply shift
               $canvas_ coords $image_($i) 0 0
               $canvas_ move $image_($i) $dx $dy
            }

            #  Final scrollregion encompasses positions of all images.
            set_scroll_region_
         }
      } msg
      if { $msg != {} } {
         info_dialog $msg
      }
   }

   #  Set the origins for FITS using CRPIX values.
   protected method set_fits_origins_ {} {
      catch {
         if { [info exists clones_(0)] } {

            #  Place mobile image at 0 0 and get image coordinates of
            #  upper left.
            $canvas_ coords $image_($mobile_) 0 0

            set bxo [$image_($mobile_) fits get CRPIX1]
            if { $bxo == {} } {
               set bxo 1
            }
            set byo [$image_($mobile_) fits get CRPIX2]
            if { $byo == {} } {
               set byo 1
            }
            set byo [expr $byo+[$image_($mobile_) height]-1]

            set_scroll_region_
            for { set i 0 } { $i < $n_ } { incr i } {

               #  Origin of upper left of this image (image coordinates).
               set xo [$image_($i) fits get CRPIX1]
               if { $xo == {} } {
                  set xo 1
               }
               set yo [$image_($i) fits get CRPIX2]
               if { $yo == {} } {
                  set yo 1
               }
               set yo [expr $yo+[$image_($i) height]-1]

               # Pixel shift from mobile image upper left to this one.
               set dx [expr $xo-$bxo]
               set dy [expr $byo-$yo]

               # Equivalent canvas shift.
               $image_($mobile_) convert dist $dx $dy image dx dy canvas

               #  Apply shift
               $canvas_ coords $image_($i) 0 0
               $canvas_ move $image_($i) $dx $dy
            }

            #  Final scrollregion encompasses positions of all images.
            set_scroll_region_
         }
      } msg
      if { $msg != "" } {
         info_dialog $msg
      }
   }

   #  Set grid origins to be the same (lower left corners).
   protected method fix_origins_ {} {
      catch {
         if { [info exists clones_(0)] } {

            #  Place mobile image at 0 0 and get image height.
            $canvas_ coords $image_($mobile_) 0 0
            set byo [$image_($mobile_) height]

            set_scroll_region_
            for { set i 0 } { $i < $n_ } { incr i } {

               #  Height of this image.
               set yo [$image_($i) height]

               #  Pixel shift from mobile image upper left to this one.
               set dy [expr $byo-$yo]

               #  Equivalent canvas shift.
               $image_($mobile_) convert dist 0 $dy image dx dy canvas

               #  Apply shift
               $canvas_ coords $image_($i) 0 0
               $canvas_ move $image_($i) $dx $dy
            }

            #  Final scrollregion encompasses positions of all images.
            set_scroll_region_
         }
      } msg
      if { $msg != {} } {
         info_dialog $msg
      }
   }

   #  Convert image coordinates to wcs coordinates in native units.
   protected method pix2wcs_ {image xo yo {radians 1}} {
      lassign [$image astpix2wcs $xo $yo 1] wcs1 wcs2
      #  Undo fudge that returns these in "degrees", the natural
      #  units are "radians". We don't use astpix2cur as this may
      #  not return decimal values (if WCS is RA/Dec may get
      #  sexagesimal).
      if { $radians } {
         set wcs1 [expr $wcs1*$d2r_]
         set wcs2 [expr $wcs2*$d2r_]
      }
      return "$wcs1 $wcs2"
   }

   #  Set the relative positions of any images using WCS information.
   #  This attempts to "orient" images (as in flip x, y or
   #  interchange), so that they can be more easily compared (or
   #  positioned in the case of the INT CCD Mosaic). Clearly fraction
   #  scale and arbitrary orientations are not supported.
   protected method set_wcs_origins_ {} {
      catch {
         if { [info exists clones_(0)] } {

            #  Place mobile image at 0 0.
            $canvas_ coords $image_($mobile_) 0 0
            set_scroll_region_

            for { set i 0 } { $i < $n_ } { incr i } {

               #  Roughly orient images.
               match_orientation_ $mobile_ $i

               #  Origin of upper left of this image (grid coordinates).
               lassign [get_image_area_ $i 0] cx0 cy0 cx1 cy1
               set cxl [expr min($cx0,$cx1)]
               set cyl [expr min($cy0,$cy1)]
               $image_($i) convert coords $cxl $cyl canvas xo yo image

               #  Convert to WCS.
               lassign [pix2wcs_ $image_($i) $xo $yo 0] wcs1 wcs2

               #  Back to pixels, but of mobile image.
               lassign [$image_($mobile_) astwcs2pix $wcs1 $wcs2] xo yo

               # Pixel shift from mobile image upper left to this one.
               set dx $xo
               set dy [expr [$image_($mobile_) height]-1.0-$yo]

               # Equivalent canvas shift.
               $image_($mobile_) convert dist $dx $dy image dx dy canvas

               #  Apply shift
               $canvas_ coords $image_($i) 0 0
               $canvas_ move $image_($i) $dx $dy
            }

            #  Final scrollregion encompasses positions of all images.
            set_scroll_region_
         }
      } msg
      if { $msg != {} } {
         info_dialog $msg
      }
   }

   #  Determine the directions that the axes of the WCS increase
   #  (different from image axes, which are always left and up).
   #  The return is a pair of values from "top", "down", "left" and
   #  "right".
   protected method get_axis_moves_ {image} {

      #  Get centre of image.
      set xc [expr [$image width]/2]
      set yc [expr [$image height]/2]

      #  Corresponding WCS coordinates (in non sexagesimal units).
      lassign [pix2wcs_ $image $xc $yc] wcs1 wcs2

      #  Get a step size. One pixel along image X axis.
      lassign [pix2wcs_ $image [expr $xc+1] $yc] wcs3 wcs4
      set d1 [expr $wcs3 - $wcs1]
      set d2 [expr $wcs4 - $wcs2]
      set step [expr max(abs($d1),abs($d2))]

      #  Increment axes to get directions.
      set wcss [expr $wcs1+$step]
      lassign [$image astcur2pix $wcss $wcs2 1] xs ys

      set d1 [expr $xs - $xc]
      set d2 [expr $ys - $yc]

      if { [expr abs($d1)] > [expr abs($d2)] } {
         # left or right
         if { $d1 >= 0.0 } {
            set xmoves "right"
         } else {
            set xmoves "left"
         }
      } else {
         # top or bottom
         if { $d2 >= 0.0 } {
            set xmoves "up"
         } else {
            set xmoves "down"
         }
      }

      set wcss [expr $wcs2+$step]
      lassign [$image astcur2pix $wcs1 $wcss 1] xs ys
      set d1 [expr $xs - $xc]
      set d2 [expr $ys - $yc]

      if { [expr abs($d1)] > [expr abs($d2)] } {
         # left or right
         if { $d1 >= 0.0 } {
            set ymoves "right"
         } else {
            set ymoves "left"
         }
      } else {
         # top or bottom
         if { $d2 >= 0.0 } {
            set ymoves "up"
         } else {
            set ymoves "down"
         }
      }
      return "$xmoves $ymoves"
   }

   #  Match the orientation of one image to another. Identify images
   #  by index.
   protected method match_orientation_ {ref target} {

      set refimage $image_($ref)
      set targetimage $image_($target)

      lassign [get_axis_moves_ $refimage] xrefori yrefori
      lassign [get_axis_moves_ $targetimage] xtarori ytarori

      set ops ""
      set x 0

      #  Match X orientations.
      switch $xrefori {
         "up" - "down" {
            switch $xtarori {
               "up" - "down" {
                  if { $xrefori != $xtarori } {
                     append ops "udf "
                  }
               }
               "left" - "right" {
                  lassign $interchanges_("$xtarori,$ytarori") xtarori ytarori
                  append ops "x "
                  set x 1
               }
            }
         }
         "left" - "right" {
            switch $xtarori {
               "up" - "down" {
                  lassign $interchanges_("$xtarori,$ytarori") xtarori ytarori
                  append ops "x "
                  set x 1
               }
               "left" - "right" {
                  if { $xrefori != $xtarori } {
                     append ops "lrf "
                  }
               }
            }
         }
      }

      # If we interchanged, then still need to orient X axis.
      if { $x } {
         switch $xrefori {
            "up" - "down" {
               if { $xrefori != $xtarori } {
                  append ops "udf "
               }
            }
            "left" - "right" {
               if { $xrefori != $xtarori } {
                  append ops "lrf "
               }
            }
         }
      }

      #  Y axis must be a flip.
      switch $yrefori {
         "up" - "down" {
            if { $yrefori != $ytarori } {
               append ops "udf "
            }
         }
         "left" - "right" {
            if { $yrefori != $ytarori } {
               append ops "lrf "
            }
         }
      }

      #  Gather orientation of reference image. This is applied to the
      #  image after the changes above. XXX could work in canvas coords?
      if { [$refimage flip x] } {
         append ops "lrf "
      }
      if { [$refimage flip y] } {
         append ops "udf "
      }
      if { [$refimage rotate] } {
         append ops "x "
      }

      #  Remove any current orientation.
      set rtdimage $target_($target)
      $rtdimage rotate 0
      $rtdimage flip x 0
      $rtdimage flip y 0
      $clones_($target).panel.info.trans update_trans

      #  Twice to really reset (seems to get lost).
      $rtdimage rotate 0
      $rtdimage flip x 0
      $rtdimage flip y 0
      $clones_($target).panel.info.trans update_trans

      #  Apply the new orientation. Repeats undo the any previous
      #  switches in that state.
      if { $ops != {} } {
         foreach op $ops {
            switch $op {
               "x" {
                  set state [$rtdimage rotate]
                  if { $state } {
                     $rtdimage rotate 0
                  } else {
                     $rtdimage rotate 1
                  }
               }
               "lrf" {
                  set state [$rtdimage flip x]
                  if { $state } {
                     $rtdimage flip x 0
                  } else {
                     $rtdimage flip x 1
                  }
               }
               "udf" {
                  set state [$rtdimage flip y]
                  if { $state } {
                     $rtdimage flip y 0
                  } else {
                     $rtdimage flip y 1
                  }
               }
            }
         }

         #  Nasty hack... to get panel buttons updated.
         $clones_($target).panel.info.trans update_trans
      }
   }

   #  Set the colour of the main canvas background.
   protected method set_background_ {colour} {
      configure -background_colour $colour
   }

   #  Toggle the visibility of the controls area.
   protected method hide_controls_ {} {
      if { $itk_option(-hide_controls) } {
         pack forget $itk_component(ControlsFrame)
      } else {
         pack $itk_component(ControlsFrame) \
            -side bottom -fill x -ipadx 5 -ipady 5
      }
   }

   #  Save all offsets to a log file.
   protected method save_offsets_ {} {
      set fid [open "GaiaOffsets.Log" w]
      puts $fid "\# GaiaBlink log file"
      puts $fid "\#"
      puts $fid "\#  File \t X-offset \t Y-offset (pixels)"
      for { set i 0 } { $i < $n_ } { incr i } {
         lassign [get_offsets_ $i] x0 y0
         puts $fid "$names_($i) \t $x0 \t $y0"
      }
      ::close $fid
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Width and height of image display area.
   itk_option define -width width Width {512}
   itk_option define -height height Height {512}

   #  Number of milliseconds to use when calculating time factors.
   itk_option define -millisec millisec MilliSec {5}

   #  Time between animations.
   itk_option define -time time Time {200}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  Whether we're using a classic view. In this only one image can
   #  be seen at a time.
   itk_option define -classic_view classic_view Classic_View 0

   #  Colour of the background box.
   itk_option define -background_colour background_colour \
      Background_Colour black {
      if { $backbox_ != {} } {
         $canvas_ itemconfigure $backbox_ -fill $itk_option(-background_colour)
         $canvas_ configure -background $itk_option(-background_colour)
      }
   }

   #  Possible colours.
   itk_option define -colors colors Colors {
      black red green blue cyan magenta yellow white grey90 grey40 grey10
   }

   #  If true images are "subsampled" when shrinking, otherwise the
   #  pixels are averaged
   itk_option define -subsample subsample Subsample 1

   #  X shared memory option
   itk_option define -usexshm usexshm Usexshm 1

   #  X synchronisation option
   itk_option define -usexsync useXsync UseXsync 1

   # If true, print diagnostic messages
   itk_option define -verbose verbose Verbose {0}

   # If true then the control area is hidden to increase the area for
   # images.
   itk_option define -hide_controls hide_controls Hide_Controls 0

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
   protected variable names_
   protected variable backbox_ {}
   protected variable stop_now_ 0
   protected variable xmark_ 0
   protected variable ymark_ 0

   #  Which views are to be shown (indexed by index).
   protected variable view_

   #  Common variables: (shared by all instances)
   #  -----------------


   #  Degrees to radians factor.
   common d2r_ 0.017453292

   #  Interchange mappings.
   common interchanges_

   set interchanges_("left,up")    "up left"
   set interchanges_("left,down")  "up right"

   set interchanges_("right,up")   "down left"
   set interchanges_("right,down") "down right"

   set interchanges_("down,left")  "right up"
   set interchanges_("down,right") "right down"

   set interchanges_("up,left")    "left up"
   set interchanges_("up,right")   "left down"

#  End of class definition.
}

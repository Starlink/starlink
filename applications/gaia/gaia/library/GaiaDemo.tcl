#+
#  Name:
#     GaiaDemo

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Controls GAIA non-interactively showing off some of its
#     capabilities and performing a test to check correct
#     installation.

#  Description:
#     This class creates a window from which the demonstration may be
#     started and in which it will be described. The type of
#     operations performed are rudimentary, but should hopefully test
#     enough functionality to check that GAIA is functioning as
#     expected and to get the attention of any watchers.

#  Invocations:
#
#        GaiaDemo object_name [configuration options]
#
#     This creates an instance of a GaiaDemo object. The return is
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
#     See itk_option define statements.

#  Methods:
#     See method definitions below.

#  Notes:
#     Some of this is a blantant hack as it assumes knowledge of the
#     names of the various components to control. Future maintainers
#     beware...

#  Inheritance:
#     TopLevelWidget

#  Copyright:
#     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
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
#     26-MAR-1998 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaDemo {}

itcl::class gaia::GaiaDemo {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate all options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: Demonstration Mode ($itk_option(-number))"

      #  Create the short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      add_short_help $itk_component(menubar).file {File menu: close window}

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Set the exit menu items.
      $File add command -label {Close window} \
         -command [code $this close] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this close]

      # Add selection for fonts.
      foreach font $fonts_ {
	 $File add command -label abc -font $font \
               -command [code $this set_font_ "$font"]
      }

      #  Add control for "rolling" demo.
      set Option [add_menubutton "Option"]
      $Option add checkbutton -label "Rolling demo" \
	  -onvalue 1 -offvalue 0 \
          -variable [scope itk_option(-rolling)]

      #  Create a text area for describing the demo. XXX must be bug
      #  in ScrollText as configuration can only be done after creation.
      itk_component add text {
         ScrollText $w_.text
      }
      set_font_ [lindex $fonts_ 1]
      $itk_component(text) configure -width 70 -height 15 -foreground blue

      #  Create the sliders bar
      itk_component add sliders {frame $w_.sliders}

      #  Add sliders to control the reading interval and
      #  general speed.
      itk_component add readtime {
	 util::LabelEntryScale $itk_component(sliders).readtime \
	       -text {Reading interval (secs):} \
	       -valuewidth 10 \
	       -labelwidth 20 \
	       -increment .5  \
	       -resolution .5 \
	       -from 3.0 \
	       -to 30.0 \
	       -show_arrows 1 \
	       -anchor w \
	       -value [expr $readtime_/1000.0] \
	       -command [code $this set_time_ read]
      }
      itk_component add pausetime {
	 util::LabelEntryScale $itk_component(sliders).pausetime \
	       -text {Pause interval (secs):} \
	       -valuewidth 10 \
	       -labelwidth 20 \
	       -increment .025  \
	       -resolution .025 \
	       -from 0.025 \
	       -to 5.0 \
	       -show_arrows 1 \
	       -anchor w \
	       -value [expr $speedtime_/1000.0] \
	       -command [code $this set_time_ pause]
      }

      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to start the demo.
      itk_component add start {
	  button $itk_component(actionframe).start -text Start \
		  -command [code $this start]
      }
      add_short_help $itk_component(start) {Start the demonstration}

      #  Add a button to stop the demonstration.
      itk_component add stop {
         button $itk_component(actionframe).stop -text Stop \
            -command [code $this stop]
      }
      add_short_help $itk_component(stop) \
	    {Stop the demonstration (after current section)}

      #  Add a button to stop the demo and close the window.
      itk_component add close {
         button $itk_component(actionframe).close -text Close \
            -command [code $this close]
      }
      add_short_help $itk_component(close) {Close the window}

      #  Pack all widgets into place.
      pack $itk_component(text) -side top -fill both -expand 1
      pack $itk_component(sliders) -side top -fill x -pady 5 -padx 5
      pack $itk_component(readtime)  -side top -fill x -pady 3 -padx 3
      pack $itk_component(pausetime)  -side top -fill x -pady 3 -padx 3
      pack $itk_component(actionframe) -side bottom -fill x -pady 5 -padx 5
      pack $itk_component(close) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(stop) -side right -expand 1 -pady 3 -padx 3
      pack $itk_component(start) -side right -expand 1 -pady 3 -padx 3
   }

   #  Destructor:
   #  -----------
   destructor  {
      catch {stop}
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Method called after initialisation.
   public method init {} {

      #  Check for demo data. This such be stored in a tar file in
      #  the main gaia_dir directory.
      global gaia_dir
      if { ! [file exists $gaia_dir/demodata.tar] } {
         error_dialog {Failed to locate demonstration files \
                       in $GAIA_DIR/demodata.tar. Cannot continue\
                       with demonstration.}
         after 0 "destroy $w_"
         return
      } else {
         set d [util::DialogWidget .#auto \
                   -title {Unpacking demo data} \
                   -text "The demonstration data files will be \
                          unpacked into the current directory \
                          ([pwd]). Are you sure that you want to do this?" \
                -buttons [list Yes No]]
         set answer [$d activate]
         if { $answer } {
            info_dialog {OK. Will not perform demonstration.}
            after 0 "destroy $w_"
            return
         } else {

            #  OK, untar data files.
            set tar_out "tar xf"
            if { [info exists env(TAR_OUT)] } {
               set tar_out $env(TAR_OUT)
            }
            eval exec $tar_out $gaia_dir/demodata.tar
         }
      }
      wm deiconify $w_
      $itk_component(text) clear all
      $itk_component(text) insert 0.0 {

                    GAIA -- Demonstration Toolbox
                    =============================

 Before starting the demonstration proper you may like to centre this
 window over the main image display and select a readable font size.

 You may also prefer to resize the main display and change the
 intervals used to slow down the demo rate and reading time.
      }
      update idletasks
   }


   #  Withdraw this window halting the demo.
   public method close {} {
      stop
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Stop the demo if running.
   public method stop {} {
      if { $running_ } {
	 set running_ 0
	 display {
	    Demo will stop after a while. Please wait...
	 }
      }
   }
   protected method really_stop_ {} {
      wm deiconify $w_
      $itk_component(text) clear all
      if { $running_ } {
	 $itk_component(text) insert 0.0 {

			 End of demonstration
			 ====================

  That's all for now. If you don't want to see it all happen again
  then press the "stop" button and wait a while (control-q in
  the main window will exit somewhat more quickly).
         }
      } else {
	 $itk_component(text) insert 0.0 {

			 End of demonstration
			 ====================

  That's all for now. If you want to see it all happen again
  then press the "start" button.
	 }
      }
      #set running_ 0
      wait_ [expr $readtime_*3]
   }

   #  Display some text in the description window, wait a moment and
   #  then proceed.
   method display {text} {
      wm deiconify $w_
      $itk_component(text) clear all
      $itk_component(text) insert 0.0 $text
      update idletasks
      wait_ $readtime_
      $itk_component(text) clear all
      wm withdraw $w_
   }

   #  Display text in the short help window of the main widget.
   method short_display {text} {
      $itk_option(-gaiamain) short_help $text
   }

   #  Set the font of main text.
   method set_font_ {font} {
      $itk_component(text) configure -font "$font"
   }

   #  Move a top-level window to the side of the display.
   protected method move_to_side_ {w} {
      set topw [winfo toplevel $w]
      wm withdraw $topw
      update idletasks
      set wwidth [winfo reqwidth $topw]
      set swidth [winfo screenwidth $topw]
      set newx [expr $swidth-$wwidth-5]
      wm geometry  $topw +$newx+5
      wm deiconify $topw
   }

   #  Start the demo.
   method start {} {
      set running_ 1
      loop_
   }

   #  Main loop method.
   protected method loop_ {} {
       while { $running_ } {
	   if { ! $quick_start_ } {
               display {

		       Welcome to the GAIA demo
		       ========================

 GAIA is an image display and analysis tool, which is distributed
 as part of the Starlink Software Collection for U.K. Astronomers.

 GAIA provides most of the usual facilities of image display tools,
 plus more astronomically useful ones such as object photometry,
 arbitrary region analysis, celestial coordinate support (i.e.
 coordinate readout, calibration and modification), grid overlays,
 blink comparison, defect patching, contouring, object detection
 and the ability to query on-line (WWW) catalogues.
                    }
               display {
 During this demonstration GAIA will be controlled non-interactively.
 So although you may interact with it (if you feel the need, but
 don't be surprised if things move on, or go horribly wrong), you
 should really just stand back and watch.
               }
               display {
 By the way, GAIA stands for:

             Graphical Astronomy and Image Analysis Tool.

 It is based on the SkyCat tool from ESO (the European Southern
 Observatory):
               }
               #  Display credits.
               global ::about_gaia
               set init [message $w_.message \
                            -justify center \
                            -text $about_gaia \
                            -borderwidth 3 \
                            -relief raised\
                            -foreground blue \
                            -font [lindex $fonts_ 1]]
               place $init -in $w_ -relx 0.5 -rely 0.5 \
		  -relwidth 1.0 -relheight 1.0 -anchor c
               wm deiconify $w_
               update idletasks
               wait_ $readtime_
               destroy $init
               wm withdraw $w_

               #  Final message is a warning for those sensitive to
               #  flashing images.
               display {

                       WARNING: FLASHING IMAGES
                       ========================

 This demonstration contains flashing images, look away if this
 may affect you.

                    }
	   }
	   # Main loop.
	   foreach demo $demolist_ {
               if { $running_ } {
		   catch { $demo }
               } else {
		   really_stop_
		   return
               }
	   }

           #  Demo runs until stopped.
           if { $itk_option(-rolling) } {
	       set running_ 1
	   } else {
	       set running_ 0
	   }
	   really_stop_
       }
   }

   #  Refresh the display and pause a while.
   protected method refresh_ {} {
      update idletasks
      wait_ $speedtime_
   }

   #  Wait a while (without blocking interface).
   protected method wait_ {millisec} {
      set continue_($this) 0
      after $millisec [code $this set_continue_]
      vwait [scope continue_($this)]
   }

   #  Set the continue_ variable to proceed after a while.
   protected method set_continue_ {} {
      set continue_($this) 1
   }

   #  Display an image in the main window.
   protected method show_image_ {file perc zoom} {
      $itk_option(-gaiamain) open $demo_dir_/$file
      $itk_option(-rtdimage) autocut -percent $perc
      $itk_option(-gaiactrl) scale $zoom $zoom
      $itk_option(-imagetrans) update_trans
   }

   #  Set the times for pausing and reading.
   protected method set_time_ {type secs} {
      if { $type == "read" } {
	 set readtime_ [expr int($secs*1000.0)]
      } else {
	 set speedtime_ [expr int($secs*1000.0)]
      }
   }

   # ---------------------
   #  Demo methods.

   #  Do simple image display, zooms etc.
   protected method basic_ {} {
      if { ! [file exists $demo_dir_/ngc1275.fits] } {
	 error "Help cannot find file ngc1275.fits; make sure \
	       you have the demonstration files available."
	 return
      }
      display {

                         Basic Image Display
                         ===================

  This demonstration will start by displaying an image (of NGC1275).
  It will then show some basic operations, such modifying the
  apparent detail through changing the minimum and maximum displayed
  values, rotation and flips for primitive reorientations and will
  finally cycle through a list of some possible false colours tables
  and intensity transfer functions.
      }
      short_display {Displaying the image...}
      show_image_ ngc1275.fits 98 2
      refresh_

      short_display {Cycling through a series different data ranges...}
      foreach p {70 80 90 98 100 98 96 94 96} {
	 $itk_option(-rtdimage) autocut -percent $p
         short_display "$p percent"
	 refresh_
      }

      short_display {Reorienting image...}
      $itk_option(-gaiactrl) flip x 1
      $itk_option(-imagetrans) update_trans
      short_display {Flipped about X axis}
      refresh_
      $itk_option(-gaiactrl) flip y 1
      $itk_option(-imagetrans) update_trans
      short_display {Flipped about Y axis}
      refresh_
      $itk_option(-gaiactrl) rotate 1
      $itk_option(-imagetrans) update_trans
      short_display {Interchanged X and Y axes}
      refresh_
      $itk_option(-gaiactrl) flip x 0
      $itk_option(-imagetrans) update_trans
      short_display {Flipped about X axis}
      refresh_
      $itk_option(-gaiactrl) flip y 0
      $itk_option(-imagetrans) update_trans
      short_display {Flipped about Y axis}
      refresh_
      $itk_option(-gaiactrl) rotate 0
      $itk_option(-imagetrans) update_trans
      short_display {Interchanged X and Y axes}
      refresh_
      #  Cycle through the front panel selections
      foreach cmap "real.lasc ramp.lasc bgyrw.lasc heat.lasc pastel.lasc" {
	 $itk_option(-rtdimage) cmap file $cmap
         refresh_
	 foreach imap "ramp.iasc equa.iasc log.iasc neg.iasc lasritt.iasc" {
            if { $imap != "null.iasc" } {
               $itk_option(-gaiactrl) itt file $imap
               short_display "Cycling through tables... cmap: $cmap, itt: $imap"
               refresh_
            }
	 }
      }
      $itk_option(-gaiactrl) cmap file real.lasc
      $itk_option(-gaiactrl) itt file ramp.iasc
      refresh_
   }

   #  Do things with the cursor and image scroll. If brief is set just do the
   #  cursor warp without comments (useful to show WCS changes).
   protected method scroll_ {{brief 0}} {
      if { ! $brief } {
	 display {

			   Zoom and Scroll
			   ===============

  Magnified parts of the image can be displayed in the special
  "zoom" window as well as in the main display.
	 }
	 $itk_option(-rtdimage) autocut -percent 90

	 short_display {Zooming in and out...}
	 foreach z {1 3 5 10 15 20 15 10 5 3} {
	    $itk_option(-gaiactrl) scale $z $z
	    $itk_option(-imagetrans) update_trans
	    short_display "Scale factor $z"
	    refresh_
	 }

      #  Centre the cursor over the image.
	 short_display {Moving cursor to change zoom display...}
      } else {
	 short_display {Scanning image...}
      }
      set width [$itk_option(-rtdimage) width]
      set height [$itk_option(-rtdimage) height]
      blt::winop warpto $itk_option(-canvas)
      $itk_option(-rtdimage) warp [expr int(-0.5*$width)] [expr int(-0.5*$height)]
      for {set i 0} {$i < $width} {incr i 5} {
         $itk_option(-rtdimage) warp 5 5
         refresh_
      }

      #  Do same effect, except scroll a very zoomed image.
      if { ! $brief } {
	 short_display {Scrolling image...}
	 $itk_option(-gaiactrl) scale 10 10
	 $itk_option(-imagetrans) update_trans
	 refresh_
	 set w [expr 1.0/double($width-1)]
	 set h [expr 1.0/double($height-1)]
	 for {set i 0} {$i < $width} {incr i 5} {
	    $itk_option(-canvas) xview moveto [expr double($i)*$w]
	    $itk_option(-canvas) yview moveto [expr double($i)*$h]
	    refresh_
	 }
	 $itk_option(-canvas) xview moveto [expr double($width/2)*$w]
	 $itk_option(-canvas) yview moveto [expr double($height/2)*$h]
	 $itk_option(-gaiactrl) scale 2 2
	 $itk_option(-imagetrans) update_trans
	 refresh_
      }
   }

   #  Slice
   protected method slice_ {} {
      display {

			  Interactive Slices
			  ==================

 A useful feature is the ability to display data "slices", which you
 can interactively adjust.
      }
      short_display {Showing image slice effects...}
      set width [$itk_option(-rtdimage) width]
      set height [$itk_option(-rtdimage) height]
      set xcen [expr $width/2]
      set ycen [expr $height/2]
      $itk_option(-rtdimage) convert coords $xcen $ycen image xcen ycen canvas
      set xcen [expr int($xcen)]
      set ycen [expr int($ycen)]
      set stride [expr int($xcen/2)]
      set x0 [expr int($xcen-$stride)]
      set y0 [expr int($ycen-$stride)]
      set x1 [expr int($xcen+$stride)]
      set y1 [expr int($ycen+$stride)]
      set id [$itk_option(-canvas) create line $x0 $y0 $x1 $y1 \
                 -fill white -width 2]
      $itk_option(-canvasdraw) add_object_bindings $id
      $itk_option(-gaiactrl) make_spectrum $id $x0 $y0 $x1 $y1
      refresh_
      move_to_side_ [winfo toplevel $itk_option(-gaiactrl).spectrum]
      wait_ $readtime_
      #  Move the line in a radial fashion, notify spectrum to update
      #  itself from the line...
      for { set i $y0 } { $i < $y1 } { incr i 3 } {
         lassign [$itk_option(-canvas) coords $id] x0 d x1 y1
         $itk_option(-canvas) coords $id $x0 $i $x1 $y1
         $itk_option(-gaiactrl).spectrum notify_cmd
	 refresh_
      }
      wait_ $readtime_
      $itk_option(-gaiactrl).spectrum quit
   }

   #  Annotations.
   protected method annotate_ {} {
      display {

			     Annotations
			     ===========

 Annotating images is useful for presentation purposes, and a range
 of simple regions and text options are available.

 Once drawn annotations can be printed, together with the image,
 to a postscript file.
      }
      short_display {Annotating image...}
      $itk_option(-rtdimage) autocut -percent 98
      $itk_option(-gaiactrl) scale 2 2
      $itk_option(-imagetrans) update_trans
      $itk_option(-gaiactrl) show_toolbox
      move_to_side_ [winfo toplevel $itk_option(-gaiactrl).draw]
      refresh_
      $itk_option(-canvas) create text 260 332 \
	-anchor w \
	-fill cyan \
	-font TkCaptionFont \
	-justify left \
	-stipple pat0 \
	-tags objects \
	-text NGC1275 \
	-width 0
      $itk_option(-canvas) create text 179 119 \
	-anchor w \
	-fill cyan \
	-font TkCaptionFont \
	-justify left \
	-stipple pat0 \
	-tags objects \
	-text NGC1278 \
	-width 0
      $itk_option(-canvas) create text 572 208 \
	-anchor w \
	-fill cyan \
	-font TkCaptionFont \
	-justify left \
	-stipple pat0 \
	-tags objects \
	-text NGC1273 \
	-width 0
      $itk_option(-canvas) create text 620 402 \
	-anchor w \
	-fill cyan \
	-font TkCaptionFont \
	-justify left \
	-stipple pat0 \
	-tags objects \
	-text NGC1272 \
	-width 0
      $itk_option(-canvas) create text 490 34 \
	-anchor w \
	-fill cyan \
	-font TkCaptionFont \
	-justify left \
	-stipple pat0 \
	-tags objects \
	-text IC907 \
	-width 0
      $itk_option(-canvas) create text 354 400 \
	-anchor w \
	-fill cyan \
	-font TkCaptionFont \
	-justify left \
	-stipple pat0 \
	-tags objects \
         -text {Slit position} \
	-width 0
      $itk_option(-canvas) create rtd_rotbox 354 348 \
	-fill yellow \
	-outline white \
	-stipple pat4 \
	-tags objects \
	-width 1 \
	-angle 135.0 \
	-semimajor 165 \
	-semiminor 10
      wait_ $readtime_
      $itk_option(-canvas) delete objects
      $itk_option(-gaiactrl).draw quit
   }

   #  Do some photometry.
   protected method photom_ {} {
      display {

			      Photometry
			      ==========

 GAIA also provides more astronomically useful facilities. Amongst
 these are aperture photometry and optimal photometry.

 In the following sequence a file of existing apertures will be read.
 Normally you would define the apertures interactively.
      }
      short_display {Displaying a new image...}
      show_image_ frame.sdf 98 1
      global gaia_library
      $itk_option(-gaiactrl) cmap file real.lasc
      refresh_
      short_display {Activating photometry toolbox...}
      $itk_option(-gaiamain) make_toolbox magphotom
      short_display {Opening file of aperture positions}
      set toolbox [$itk_option(-gaiamain) component magphotom]
      $toolbox read_file $demo_dir_/plainphotom.dat
      refresh_
      move_to_side_ $toolbox
      wait_ $readtime_
      display {
 The apertures will now be measured. Notice the magnitudes, sky
 estimates etc. that appear in the object details fields. All object
 measurements can be viewed under the Results tab.

 If necessary the object background can be determined from
 non-annular regions and the objects themselves may be measured
 in elliptical apertures.
      }
      short_display {Measuring aperture magnitudes...}
      $toolbox measure_objects [code $this set_continue_]
      vwait [scope continue_($this)]
      short_display {done...}
      wait_ $readtime_
      $toolbox save_objects photom.tmp
      update idletasks
      $toolbox close
   }

   #  Do regions manipulations.
   protected method regions_ {} {
      display {

			    Image Regions
			    =============

 Working with regular or arbitrary shaped regions can be useful
 when dealing with real-world objects and backgrounds. GAIA provides
 the ability to  define such regions and then get statistics, or,
 remove or extract them from the image.
      }
      display {
 In this example the regions shown on the image will be drawn
 non-interactively, normally you would define these using the
 display cursor (regions can be resized and moved).
      }
      short_display {Displaying an image...}
      show_image_ frame.sdf 90 1
      global gaia_library
      $itk_option(-gaiactrl) cmap file real.lasc
      refresh_
      short_display {Activating regions toolbox...}
      $itk_option(-gaiamain) make_toolbox ard
      short_display {Opening file of region descriptions}
      set toolbox [$itk_option(-gaiamain) component ard]
      $toolbox read_file $demo_dir_/regions.dat
      refresh_
      move_to_side_ $toolbox
      wait_ $readtime_
      short_display {Measuring statistics...}
      $toolbox stats all [code $this set_continue_]
      vwait [scope continue_($this)]
      wait_ $readtime_
      short_display {Removing regions...}
      $toolbox configure -replace 1
      $toolbox blank all [code $this set_continue_]
      vwait [scope continue_($this)]
      wait_ $readtime_
      $itk_option(-rtdimage) autocut -percent 70
      $itk_option(-gaiamain) configure -temporary 0
      wait_ $readtime_
      update idletasks
      $toolbox close
   }

   #  Patch the image.
   protected method patch_ {} {
      display {

			       Patching
			       ========

 An interesting feature (of occasionally dubious application!) is
 that you can "patch" your image, removing any defects.

 The shapes of the regions you can remove may be arbitrary, as are
 those used to estimate replacement values.
      }
      short_display {Displaying an image...}
      show_image_ frame.sdf 90 3
      global gaia_library
      $itk_option(-gaiactrl) cmap file real.lasc
      refresh_
      short_display {Activating patch toolbox...}
      $itk_option(-gaiamain) make_toolbox patch
      short_display {Opening region description}
      set toolbox [$itk_option(-gaiamain) component patch]
      $toolbox read_file repl $demo_dir_/patch.dat
      refresh_
      move_to_side_ $toolbox
      wait_ $readtime_
      short_display {Patching image...}
      $toolbox do_patch
      wait_ $readtime_
      short_display {Restoring image...}
      $toolbox undo
      update idletasks
      wait_ $readtime_
      short_display {Patching image again (quickly)...}
      for {set i 0} {$i < 10} {incr i} {
	 $toolbox do_patch
	 refresh_
	 $toolbox undo
	 refresh_
      }
      $toolbox set_modified 0
      $toolbox close
   }

   #  Contouring.
   protected method contour_ {} {
      display {

			      Contouring
			      ==========

 Contouring can be done of the displayed image, or any other image
 over the displayed image.

 The first example shows an image contoured by its own data. The
 following show contours derived from a radio image and an infrared
 image.
      }
      short_display {Displaying an image...}
      show_image_ m51_opt.sdf 98 1
      global gaia_library
      $itk_option(-gaiactrl) cmap file real.lasc
      refresh_
      short_display {Activating contour toolbox...}
      $itk_option(-gaiamain) make_toolbox contour
      set toolbox [$itk_option(-gaiamain) component contour]
      $toolbox read_config $demo_dir_/m51_opt.cont
      refresh_
      move_to_side_ $toolbox
      wait_ $readtime_
      short_display {Contouring image...}
      $toolbox redraw 1
      wait_ $readtime_
      short_display {Contouring radio image...}
      $toolbox read_config $demo_dir_/m51_radio.cont
      $toolbox redraw 1
      wait_ $readtime_
      short_display {Contouring infrared image...}
      $toolbox read_config $demo_dir_/m51_iras.cont
      $toolbox redraw 1
      wait_ $readtime_
      $toolbox close
   }

   #  Object detection
   protected method detection_ {} {
      display {

		      Automated Object Detection
		      ==========================

 Automated object detection and analysis is performed using a toolbox
 that controls the SExtractor (source extractor) program.

 The results are shown in a catalogue window as detection ellipses.

      }
      short_display {Displaying an image...}
      show_image_ ngc1275.fits 90 2
      global gaia_library
      $itk_option(-gaiactrl) cmap file real.lasc
      refresh_
      short_display {Activating SExtractor toolbox...}
      $itk_option(-gaiamain) make_toolbox sextractor
      set toolbox [$itk_option(-gaiamain) component sextractor]
      refresh_
      move_to_side_ $toolbox
      refresh_
      wait_ $readtime_
      short_display {Detecting images...}
      $toolbox run
      wait_ $readtime_
      $toolbox close
      set catname [$toolbox get_catname]
      $catname close
      $itk_option(-canvasdraw) clear
   }

   #  Blink images.
   protected method blink_ {} {
      display {

			   Blink Comparison
			   ================

 It is possible to display more than one image at a time. Each of
 these can be compared with the others using a "blink comparision"
 toolbox.

 Images in the blink toolbox can be moved about relative to each
 other.
      }
      show_image_ frame.sdf 90 1
      set clone [$itk_option(-gaiamain) noblock_clone "" "$demo_dir_/frame.sdf(5:,5:)"]
      set cloneimg [[$clone get_image] get_image]
      update
      $clone make_toolbox blink
      set toolbox [$clone component blink]
      $cloneimg autocut -percent 95
      short_display {Starting animation...}
      $toolbox animate_on
      $toolbox change_speed 12
      wait_ $readtime_
      short_display {Increasing speed (/2)}
      $toolbox change_speed 6
      wait_ $readtime_
      short_display {Increasing speed (/4)}
      $toolbox change_speed 3
      wait_ $readtime_
      $toolbox animate_off
      delete object $toolbox
      delete object $clone
   }

   #  Display an astrometry grid.
   protected method grid_ {} {
      display {

			   Coordinate Grids
			   ================

  Using GAIA you can overlay coordinate grids on your images.
  For suitable images (i.e. those with astrometric calibrations)
  you can draw these in a variety of celestial coordinate systems.
      }
      short_display {Displaying an image...}
      show_image_ ngc1275.fits 95 1
      global gaia_library
      $itk_option(-gaiactrl) cmap file real.lasc
      refresh_
      short_display {Drawing grid...}
      $itk_option(-gaiamain) make_toolbox astgrid
      set toolbox [$itk_option(-gaiamain) component astgrid]
      refresh_
      move_to_side_ $toolbox
      refresh_
      $toolbox read_options $demo_dir_/default.opt
      $toolbox draw_grid 1
      wait_ $readtime_
      $toolbox read_options $demo_dir_/fk4.opt
      $toolbox draw_grid 1
      wait_ $readtime_
      $toolbox read_options $demo_dir_/fk5.opt
      $toolbox draw_grid 1
      wait_ $readtime_
      $toolbox read_options $demo_dir_/ecliptic.opt
      $toolbox draw_grid 1
      wait_ $readtime_
      $toolbox read_options $demo_dir_/galactic.opt
      $toolbox draw_grid 1
      wait_ $readtime_
      display {
 Grids can also be drawn in various celestial projections and
 "difficult" regions of the sky.
      }
      $toolbox read_options $demo_dir_/default.opt
      short_display {Displaying all sky-projection image...}
      show_image_ cobe.fit 95 2
      $toolbox draw_grid 1
      wait_ $readtime_
      short_display {Displaying region near FK5/J2000 north pole}
      show_image_ nearpole.fits 99 1
      $toolbox draw_grid 1
      wait_ $readtime_
      display {
 There are also many different options to control the look of the
 grid.
      }
      show_image_ cobe.fit 95 2
      $toolbox read_options $demo_dir_/cobe.opt
      $toolbox draw_grid 1
      wait_ $readtime_
      short_display {Zooming in and out of grid overlay}
      foreach zoom { 3 5 7 9 11 13 15 17 19 17 15 13 11 9 7 5 3 } {
	 $itk_option(-gaiactrl) scale $zoom $zoom
	 $itk_option(-imagetrans) update_trans
	 refresh_
      }
      wait_ $readtime_

      $toolbox read_options $demo_dir_/cobe_bit.opt
      short_display {Drawing grid just over zoomed part of image}
      $toolbox remove_grid
      $itk_option(-gaiactrl) scale 3 3
      $toolbox draw_grid 1
      $itk_option(-imagetrans) update_trans
      wait_ $readtime_
      $toolbox close
   }

   #  Define an astrometry solution to image.
   protected method astdefine_ {} {
      display {

		  Entering a known Image Astrometry
		  =================================

 Using GAIA you can define an astrometry calibration for your image.
 After this is applied it can be saved permanently with the image
 and used to read-off coordinates and plot grids.
      }
      short_display {Displaying an image without any astrometry...}
      show_image_ ngc1275_nowcs.fits 95 1
      display {
 Note that this image does not at present have an astrometry
 calibration, as shown by the lack of values in the ra and dec
 fields in the main control panel during the following scan.
      }
      scroll_ 1
      $itk_option(-gaiamain) make_toolbox astdefine
      set toolbox [$itk_option(-gaiamain) component astdefine]
      refresh_
      move_to_side_ $toolbox
      refresh_
      [$toolbox component crpix1] configure -value 176
      [$toolbox component crpix2] configure -value 177
      [$toolbox component crval1] configure -value {03:19:48.3}
      [$toolbox component crval2] configure -value {41:30:42.23}
      [$toolbox component cdelt1] configure -value -1.7
      [$toolbox component cdelt2] configure -value 1.7
      [$toolbox component crota2] configure -value -2.0
      display {
 Using the values in the toolbox an astrometric calibration
 will now be assigned to the image. This will be demonstrated
 by drawing a sky coordinates grid overlay and by moving the
 cursor over the image to display ra and dec values in the
 control panel.
      }
      $toolbox test
      refresh_
      $itk_option(-gaiamain) make_toolbox astgrid
      set plotbox [$itk_option(-gaiamain) component astgrid]
      refresh_
      move_to_side_ $plotbox
      refresh_
      $plotbox read_options $demo_dir_/default.opt
      short_display {Drawing astrometry grid...}
      $plotbox draw_grid 1
      refresh_
      scroll_ 1
      $plotbox close
      wait_ $readtime_
      $toolbox cancel
   }

   #  Now do the same with reference coordinates.
   protected method astreference_ {} {
      display {

	  Astrometric Calibration using Reference Positions
	  =================================================

 You can also calibrate the astrometry of your image using reference
 positions. In the following some reference positions will be read
 in from a file and displayed. Normally you would add the reference
 positions interactively.
      }
      short_display {Displaying an image without any astrometry...}
      show_image_ ngc1275_nowcs.fits 95 2
      $itk_option(-gaiamain) make_toolbox astreference
      set toolbox [$itk_option(-gaiamain) component astreference]
      refresh_
      move_to_side_ $toolbox
      refresh_
      set table [$toolbox component table]
      $table read_positions $demo_dir_/ngc1275.astrom
      wait_ $readtime_
      display {
 Using the reference positions an astrometric calibration will be
 determined for the image. This will be demonstrated by drawing a
 sky coordinates grid overlay and by moving the cursor over the
 image to display ra and dec values in the control panel.

 Note the image scale and fit quality estimate shown in the toolbox.
      }
      short_display {Centroiding positions...}
      $table centroid
      $toolbox test
      refresh_
      $itk_option(-gaiamain) make_toolbox astgrid
      set plotbox [$itk_option(-gaiamain) component astgrid]
      refresh_
      $plotbox close
      $plotbox read_options $demo_dir_/default.opt
      short_display {Drawing astrometry grid...}
      $plotbox draw_grid 1
      scroll_ 1
      wait_ $readtime_
      $plotbox close
      $toolbox cancel
   }

   #  Reference some not quite accurate positions.
   protected method astrefine_ {} {
      display {

	     Modifying existing Astrometric Calibrations
	     ===========================================

 If the astrometric calibration of your image isn't completely
 accurate you can "refine" it by adding in a small change.
      }
      display {
 Reference positions can be adjusted individually, or you can
 move/scale all the reference positions globally.

 For this demonstration an approximate calibration will be added to
 the image, which will then be refined by adding in a shift and
 rotation. These positions will then be corrected further by
 centering on the nearest intensity peaks.
      }
      short_display {Displaying an image without any astrometry...}
      show_image_ ngc1275_nowcs.fits 95 3
      $itk_option(-gaiamain) make_toolbox astdefine
      set toolbox [$itk_option(-gaiamain) component astdefine]
      refresh_
      move_to_side_ $toolbox
      refresh_
      [$toolbox component crpix1] configure -value 175
      [$toolbox component crpix2] configure -value 173
      [$toolbox component crval1] configure -value {03:19:48.3}
      [$toolbox component crval2] configure -value {41:30:42.23}
      [$toolbox component cdelt1] configure -value -1.7
      [$toolbox component cdelt2] configure -value 1.7
      [$toolbox component crota2] configure -value -0.0
      short_display {Setting an approximate astrometric solution...}
      refresh_
      $toolbox accept
      $itk_option(-gaiamain) make_toolbox astgrid
      set plotbox [$itk_option(-gaiamain) component astgrid]
      refresh_
      $plotbox close
      $plotbox read_options $demo_dir_/default.opt
      short_display {Drawing astrometry grid...}
      $plotbox draw_grid 1
      refresh_
      display {
 Now the image has an approximate solution some reference positions
 will displayed from a file (these could equally well be derived from
 an on-line catalogue at this stage). Note that a slight offset
 and rotation are required.
      }
      $itk_option(-gaiamain) make_toolbox astrefine
      set toolbox [$itk_option(-gaiamain) component astrefine]
      refresh_
      move_to_side_ $toolbox
      refresh_
      set table [$toolbox component table]
      $table read_positions $demo_dir_/ngc1275.astrom
      $table update_x_and_y
      refresh_
      wait_ $readtime_
      short_display {Shifting by 1 pixel in X and 4 pixels in Y...}
      $table offset 1 4
      wait_ $readtime_
      short_display {Rotating by 2 degrees...}
      $table rotate -2.0
      wait_ $readtime_
      $table centroid
      short_display {Centroiding positions for additional accuracy...}
      wait_ $readtime_
      $toolbox refine
      wait_ $readtime_
      display {
 The refinements have now been combined into the astrometric
 calibration of the image.
      }
      $plotbox draw_grid 1
      scroll_ 1
      wait_ $readtime_
      $plotbox close
      $toolbox cancel
   }

   #  Copy a solution from another image.
   protected method astcopy_ {} {
      display {

		   Copying Astrometric Calibrations
		   ================================

 The final way in which you add an astrometry calibration to your
 image is by copying one from another image.

 This can be useful say if you have a series of dithered images,
 as you can refine the solution for each image in turn (by adding
 in a shift).
      }
      short_display {Displaying an image without any astrometry...}
      show_image_ ngc1275_nowcs.fits 95 3
      scroll_ 1
      short_display {Copying solution from similar image...}
      $itk_option(-gaiamain) make_toolbox astcopy
      set toolbox [$itk_option(-gaiamain) component astcopy]
      refresh_
      move_to_side_ $toolbox
      refresh_
      $toolbox default_file $demo_dir_/ngc1275.fits
      $toolbox test
      display {
  Astrometry has now been copied from another image.
      }
      scroll_ 1
      $toolbox cancel
   }

   #  Display show results from on-line catalogues. Need to
   #  get our files shown, rather than a via file selection
   #  dialog, hence messing about.
   protected method skycat_ {} {

      #  Get access to catalogues so we can list all those available.
      astrocat $w_.cat

      display {

			  On-line Catalogues
			  ==================

  Using GAIA you can obtain lists of objects from catalogues on the
  internet. If these are located within the bounds of your image
  these will be show as various markers.
      }
      display {
  In the following examples the results of querying some of these
  catalogues will be read back from local files. Normally you query
  these using your local internet access.
      }
      set catalogs  [lsort [$w_.cat info catalog]]
      set ncats [llength $catalogs]
      set catlist   ""
      for {set i 0} {$i < $ncats } {incr i} {
	 append catlist "      [lindex $catalogs $i]\n"
      }
      display "   Some of the currently available catalogues are:
$catlist
      "
      $w_.cat delete
      show_image_ ngc1275.fits 95 2
      display {
 The results of a query on the NGC1275 field for the HST Guide Star
 Catalogue will now be displayed.

 Note these results can be used as reference positions when
 determining an astrometric calibration.
      }
      set toolbox [cat::AstroCat::new_catalog $demo_dir_/ngc1275_gsc.tab \
                      $itk_option(-gaiactrl) ::gaia::GaiaSearch 0 0 local]

      #  Get name of catalogue window. This is the one that is
      #  displaying our file.
      if { $toolbox == {} } {
         set toolbox [find_skycat_ ngc1275_gsc.tab]
         if { $toolbox == "" } {
            return
         }
      }
      refresh_
      move_to_side_ $toolbox
      refresh_
      wait_ $readtime_

      #  Highlight some objects.
      short_display {Selecting an object...}
      $itk_option(-rtdimage) convert coords 160 160 image x0 y0 canvas
      $itk_option(-rtdimage) convert coords 190 190 image x1 y1 canvas
      $toolbox select_region $x0 $y0 $x1 $y1
      wait_ $readtime_

      #  Select a table row and label it.
      short_display {Labelling object...}
      set table [$toolbox get_table]
      $table select_row 0
      $toolbox label_selected_row
      $itk_option(-canvas) itemconfigure ngc1275_gsc.tab -fill green
      wait_ $readtime_
      $toolbox clear

      #  Now reveal NED results.
      display {
 You can also get results from the NED database (which for instance
 allows you to identify any known extragalactic objects on your
 image).
      }
      $toolbox quit
      set toolbox [cat::AstroCat::new_catalog $demo_dir_/ngc1275_ned.tab \
                      $itk_option(-gaiactrl) ::gaia::GaiaSearch 0 0 local]

      #  Get name of catalogue window.
      if { $toolbox == {} } {
         set toolbox [find_skycat_ ngc1275_ned.tab]
         if { $toolbox == "" } {
            return
         }
      }
      refresh_
      move_to_side_ $toolbox
      refresh_

      #  Highlight some objects.
      short_display {Selecting an object...}
      $itk_option(-rtdimage) convert coords 160 160 image x0 y0 canvas
      $itk_option(-rtdimage) convert coords 190 190 image x1 y1 canvas
      $toolbox select_region $x0 $y0 $x1 $y1
      wait_ $readtime_

      #  Select table rows and label them.
      short_display {Labelling objects...}
      set table [$toolbox get_table]
      foreach row { 0 13 16 19 25 29 } {
	 $table select_row $row
	 $toolbox label_selected_row
      }
      $itk_option(-canvas) itemconfigure ngc1275_ned.tab -fill yellow
      wait_ $readtime_
      $toolbox clear
      $toolbox quit
   }

   #  Other on-line capabilities. Archives and Image servers.
   protected method archives_ {} {
      display {

		      Archive and Image Stores.
		      =========================

 Using GAIA you can also query some on-line data archives and
 image stores (i.e. the Digital Sky Survey).

 The data archives available cover the HST, NTT and CFHT.
      }
      display {
 In the following screens an example of a query about HST data
 available for the NGC1275 field will be shown.
      }
      show_image_ ngc1275.fits 95 2
      set toolbox [cat::AstroCat::new_catalog $demo_dir_/ngc1275_hst.tab \
                      $itk_option(-gaiactrl) ::gaia::GaiaSearch 0 0 local]

      #  Get name of catalogue window.
      if { $toolbox == {} } {
         set toolbox [find_skycat_ ngc1275_hst.tab]
         if { $toolbox == "" } {
            return
         }
      }
      refresh_
      move_to_side_ $toolbox
      refresh_
      wait_ $readtime_
      display {
 If publicly available you can get a preview of the HST data pulled
 directly from the archive.
      }
      show_image_ preview.sdf 95 -2

      $itk_option(-gaiamain) make_toolbox astgrid
      set plotbox [$itk_option(-gaiamain) component astgrid]
      refresh_
      $plotbox close
      $plotbox read_options $demo_dir_/default.opt
      short_display {Drawing astrometry grid...}
      $plotbox draw_grid 1

      wait_ $readtime_
      $plotbox close
      $toolbox clear
      $toolbox quit
   }

   #  Get the name of the SkyCat instance that is displaying the
   #  current local catalogue.
   protected method find_skycat_ {filename} {
      set toolbox ""
      foreach w [cat::AstroCat::instances] {
         if { [string match "*${filename}" [$w cget -catalog]] } {
            set toolbox $w
            break;
	 }
      }
      if { $toolbox == "" } {
	 error_dialog {Failed to connect to catalogue window}
      }
      return $toolbox
   }


   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the main GAIA window invoking this.
   itk_option define -gaiamain gaiamain GaiaMain .rtd0 {
      configure -imagetrans $itk_option(-gaiamain).image.panel.info.trans
      configure -gaiactrl $itk_option(-gaiamain).image
      configure -canvasdraw $itk_option(-gaiamain).image.draw
      configure -canvas $itk_option(-gaiamain).image.imagef.canvas
      configure -rtdimage [$itk_option(-gaiactrl) get_image]
   }

   #  Name of starrtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Name of GaiaImageCtrl widget.
   itk_option define -gaiactrl gaiactrl GaiaCtrl {}

   #  Name of draw widget.
   itk_option define -canvasdraw canvasdraw CanvasDraw {}

   #  Name of main canvas.
   itk_option define -canvas canvas Canvas {}

   #  Name of image transform widget.
   itk_option define -imagetrans imagetrans ImageTrans {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Is demo "rolling"?
   itk_option define -rolling rollong Rolling 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  Names of available fonts.
   protected variable fonts_ {
      -*-courier-bold-r-*-*-*-120-*-*-*-*-*-*
      -*-courier-bold-r-*-*-*-140-*-*-*-*-*-*
      -*-courier-bold-r-*-*-*-180-*-*-*-*-*-*
      -*-courier-bold-r-*-*-*-240-*-*-*-*-*-*
   }

   #  State of demo.
   protected variable running_ 0

   #  List of all known demos.
    protected variable demolist_ \
        "basic_ scroll_ slice_ annotate_ photom_ regions_ patch_ \
         contour_ detection_ blink_ grid_ astdefine_ astreference_ \
         astrefine_ astcopy_ skycat_ archives_"

   #  Interval to wait while reading text
   protected variable readtime_ 15000

   #  Interval to slow down demo.
   protected variable speedtime_ 100

   #  Name of cloned window.
   protected variable clone_ {}

   #  Directory for demo files.
   protected variable demo_dir_ [pwd]

   #  Development mode.
   protected variable quick_start_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Variable to watch while waiting.
   common continue_

#  End of class definition.
}

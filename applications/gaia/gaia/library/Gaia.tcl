#+
#  Name:
#     Gaia.tcl

#  Purpose:
#     Defines a class for creating a GAIA window.

#  Type of Module:
#     [incr Tk] class

#  Description:
#     This is the class that creates the GAIA display tool.

#  Invocation:
#     Gaia name [configuration options]

#  Notes:
#     This will only run with the gaia_wish installed as part
#     of the GAIA package with a Starlink extended RTD.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Inherits:
#     Methods and configuration options of SkyCat (and Rtd).

#  History:
#     24-SEP-1997 (PDRAPER):
#        Original version
#     10-MAR-1998 (PDRAPER):
#        Clone method now accepts a file name and additional options.
#     06-APR-1998 (PDRAPER):
#        Added demo toolbox.
#     07-APR-1998 (PDRAPER):
#        Moved temporary code to GaiaImageCtrl.
#     09-APR-1998 (PDRAPER):
#        Changed clone method to not use TopLevelWidget::start.
#        This simplifies passing on new options and makes it
#        possible to wait for the window to appear (start blocks
#        with a tkwait which means that it is impossible to
#        work out when the clone is running).
#     10-JUL-1998 (PDRAPER):
#        Added changes to support cloning of toolboxes.
#     10-SEP-1998 (PDRAPER):
#        Added SExtractor toolbox.
#     10-MAR-1999 (PDRAPER):
#        Attempt merge of GAIA plugin differences...
#     {enter_changes_here}

#-

#  Version.
set gaia_version "@GAIA_VERSION@"

#  Make a local copy of about_skycat so we can divert bug reports.
set about_gaia "\

GAIA version $gaia_version

Copyright (C) 1997-1999 Central Laboratory of the Research Councils (U.K.)

Authors:
Peter W. Draper (P.W.Draper@durham.ac.uk)

GAIA is derived from SkyCat version [skycat_version]
Copyright (C) 1996-1999 ESO - European Southern Observatory

Authors:
Allan Brighton (abrighto@eso.org)
Thomas Herlin (therlin@eso.org)
Miguel Albrecht (malbrech@eso.org)
Daniel Durand (durand@dao.nrc.ca)
Peter Biereichel (pbiereic@eso.org)

Bug reports and suggestions to: ussc@star.rl.ac.uk

"

set about_skycat ""

set gaia_usage {
Usage: gaia ?fitsFile? ?-option value ...?

Options:
 -colorramp_height <n>    - height of colorramp window (default: 12).
 -float_panel <bool>      - put info panel in a popup window (default: 0).
 -panel_layout <layout>   - panel layout, one of: "saoimage", "reverse" or "default" .
 -pickobjectorient <v>    - orientation for pick object win: "horizontal", "vertical"    
 -min_scale <n>           - minimum scale for magnification menu (default: -10).
 -max_scale <n>           - maximum scale for magnification menu (default: 20).
 -remote <bool>           - Use existing skycat process, if available, with Tk send.
 -debug <bool>            - debug flag: run bg processes in fg.
 -default_cmap <cmap>     - default colormap.
 -default_itt <itt>       - default intensity transfer table.
 -file <file>             - fits file to load ('-' for stdin).
 -port <port>             - Listen for remote cmds on port (default: 0 = choose port).
 -scrollbars <bool>       - Display scrollbars (not displayed by default).
 -shm_data <bool>         - Put image data in sysV shared memory.
 -shm_header <bool>       - Put image header in sysV shared memory.
 -usexshm <bool>          - Use X shared mem, if available (default).
 -use_zoom_view <bool>    - Use a "view" of the image for the zoom window (default).
 -verbose <bool>          - Print diagnostic messages.
 -with_colorramp <bool>   - Display the color bar (default).
 -with_pan_window <bool>  - Display the pan window (default).
 -with_zoom_window <bool> - Display the zoom window (default).
 -zoom_factor <n>         - zooming factor (default: 4).
}

itk::usual Gaia {}

#  Create a class for the application.
class gaia::Gaia {
   inherit skycat::SkyCat

   #  Constructor: create a toplevel window.
   constructor {args} {

      #  And start things going.
      configure -center 0

      #  Remove any options we're overriding and evaluate all
      #  options.
      itk_option remove rtd::Rtd::scrollbars
      itk_option remove rtd::Rtd::panel_layout
      eval itk_initialize $args

      #  Override about_skycat message.
      global about_skycat about_gaia
      set about_skycat $about_gaia
   }

   #  Destructor:
   destructor {
   }

   #  Remove a window, if this is the last then remove . well.
   public method remove { kill } {

      #  Remove all top-level widgets associated with this object.
      #  These are not always cleared up by itk...
      foreach w [array name itk_component] {
         if { [winfo exists $itk_component($w)] &&
              [winfo toplevel $itk_component($w)] != "$w_" } {
            destroy $itk_component($w)
         }
      }

      #  Now destroy the main window.
      destroy $w_
      incr clone_cnt -1
      if { $clone_cnt < 0 || $kill } {
         destroy .
      }
   }

   #  Called after the options have been evaluated. Note this method is
   #  a hybrid of the Rtd/SkyCat inits.
   public method init {} {
      wm withdraw $w_

      #  Intercept window manager delete and close down correctly
      #  (as in the Exit option).
      wm protocol $w_ WM_DELETE_WINDOW [code $this remove 0]

      #  Try to fit to the available viewing area. Assumes fits onto
      #  anything taller than 880 naturally (i.e. Sparc console), and
      #  has a width of at least 740 pixels (i.e. will do 800x600).
      set sheight [winfo screenheight $w_]
      if { $sheight < 880 } {
	 set newheight [expr int($sheight*0.95)] ;# Leave little room at top/bottom
	 wm geometry $w_ 740x${newheight}
      }

      #  Get the clone number for this window.
      set clone_ $itk_option(-number)
      #regsub {\.rtd} $w_ {} clone_

      #  Set/get X defaults - can be overridden in subclass and/or
      #  in user's .Xdefaults file.
      tk appname GAIA
      util::setXdefaults
      Rtd::setXdefaults
      cat::setXdefaults
      skycat::setXdefaults
      gaia::setXdefaults

      #  Start the introduction window going.
      set Init_ [TopLevelWidget $w_.init -center 1 \
                    -background red -cursor watch]
      wm overrideredirect $Init_ 1
      global env about_skycat
      set Message [message $Init_.msg -text $about_skycat \
                       -justify center \
                       -width 6i \
                       -borderwidth 2 -relief groove]
      set Progress_ [ProgressBar $Init_.progress \
                        -from 0 -to 12 -value 0 \
                        -borderwidth 5 -relief ridge -cursor watch]
      pack $Message -side top -fill x -padx 2m -pady 2m
      pack $Progress_ -side bottom -fill x -padx 2m -pady 2m
      tkwait visibility $Init_

      #  Now start the interface.
      global gaia_version
      feedback "\nGAIA/SkyCat ($clone_) $gaia_version initializing...\n\n"
      wm title $w_ "GAIA/SkyCat ($clone_)"
      wm iconname $w_ "GAIA/SkyCat ($clone_)"
      feedback "making image window..."

      #  Create the rtd image widget and exit on errors, such as no more
      #  colors, no more memory...
      make_rtdimage

      #  Set rtd camera if not set on command line default to
      #  environment variable.
      global ::env
      if {"$itk_option(-camera)" == ""} {
         if {[info exists env(RTD_CAMERA)]} {
            config -camera $env(RTD_CAMERA)
         } else {
            config -camera RTDSIMULATOR
         }
      }

      #  Display the image also as an icon.
      feedback "Icon..."
      if {$itk_option(-disp_image_icon)} {
         itk_component add icon {
            RtdImageIcon $w_.icon \
               -image $itk_component(image) \
               -usexshm $itk_option(-usexshm) \
               -verbose $itk_option(-verbose) \
               -subsample $itk_option(-subsample) \
               -center 0
         }
         wm iconwindow $w_ $w_.icon

         #  On openwindows iconwindows are displayed but do not
         #  redirect events, so add a fake deiconify binding.
	 bind $itk_component(icon) <Double-1> "wm deiconify $w_"
      }

      #  Add the menubars and short help.
      feedback "menubar..."
      add_menubar
      make_short_help
      add_help_menu

      feedback "GAIA toolboxes..."
      if { $itk_option(-gaia) } {
         add_gaia_menu
      }
      feedback "real time menus..."
      if { ! $itk_option(-rtd) } {
         pack forget $itk_component(menubar).real-time
      }
      feedback "catalogue menus..."
      if { $itk_option(-cat) } {
         AstroCat::add_catalog_menu $w_ [code $image_] ::gaia::GaiaSearch \
            $itk_option(-debug)
         set m "[get_menubutton "Data-Servers"].m"
         add_menuitem $m command "Load ESO config file..."  \
            "Load the default ESO catalog config file" \
            -command [code $this load_eso_config]
      }

      #  Add the filters menu if required (not used at present).
      feedback "filters..."
      if { $itk_option(-filters) } {
         make_filters_menu
      }
      pack $itk_component(image) -fill both -expand 1

      #  Add the SkyCat graphics features (really a plugin, but we're
      #  now using these yet).
      add_graphics_features $w_

      # make a message if we are using a private colormap
      if {[$image_ cmap isprivate]} {
         catch {
            puts stderr \
               "Unable to allocate enough colors for image display\
             in the default colormap - using a private colormap. If this\
             causes any problems with color flashing, try exiting other\
             color intensive applications (such as netscape) first and then\
             restarting. (Tip: use `netscape -ncols 60' to start netscape)\n"
         }
      }

      #  Remove the introduction window.
      destroy $Init_
      wm deiconify $w_
   }

   # this method can be redefined in a subclass to get feedback during
   # startup
   public method feedback {msg} {
      if { [winfo exists $Progress_] } {
         $Progress_ configure -text $msg -value [incr sofar_]
         update idletasks
      }
   }

   #  Add help for GAIA and SkyCat.
   public method add_help_menu {} {
      global env
      set m [add_help_button $env(GAIA_DIR)/Gaia.hlp "On Window..." \
             {Display help on this window and general features}   ]

      add_menuitem $m command "About GAIA/SkyCat..." \
         {Display a window with information about this GAIA/SkyCat version} \
         -command [code $itk_component(image) about]

      add_menuitem $m command "SkyCat..." \
         {Display information about SkyCat in netscape (if netscape is available)} \
         -command [code $itk_component(image) send_to_netscape $itk_option(-help_url)]

      add_short_help $itk_component(menubar).help \
         {Help menu: display information about this application}
   }

   #  Set default X resources for colors and fonts, and set some default key
   #  Bindings.
   public method setXdefaults {} {
      skycat::setXdefaults
      gaia::setXdefaults
   }

   #  Create the rtd image widget with the extended RTD functionality
   #  needed by GAIA.
   public method make_rtdimage {} {
      set image_ $w_.image
      itk_component add image {
         GaiaImageCtrl $image_ \
            -file $itk_option(-file) \
            -file_change_cmd [code $this configure -file] \
            -file_types $itk_option(-file_types) \
            -usexshm $itk_option(-usexshm) \
            -verbose $itk_option(-verbose) \
            -shm_header $itk_option(-shm_header) \
            -shm_data $itk_option(-shm_data) \
            -min_colors $itk_option(-min_colors) \
            -max_colors $itk_option(-max_colors) \
            -drag_scroll $itk_option(-drag_scroll) \
            -scrollbars $itk_option(-scrollbars) \
            -subsample $itk_option(-subsample) \
            -use_zoom_view $itk_option(-use_zoom_view) \
            -zoom_view_propagate $itk_option(-zoom_view_propagate) \
            -with_zoom_window $itk_option(-with_zoom_window) \
            -dozoom $itk_option(-dozoom) \
            -with_pan_window $itk_option(-with_pan_window) \
            -zoom_factor $itk_option(-zoom_factor) \
            -zoom_width $itk_option(-zoom_width) \
            -zoom_height $itk_option(-zoom_height) \
            -pan_width $itk_option(-pan_width) \
            -pan_height $itk_option(-pan_height) \
            -colorramp_height $itk_option(-colorramp_height) \
            -default_cmap $itk_option(-default_cmap) \
            -default_itt $itk_option(-default_itt) \
            -with_colorramp $itk_option(-with_colorramp) \
            -feedback [code $this feedback] \
            -port $itk_option(-port) \
            -shorthelpwin $this \
            -debug $itk_option(-debug) \
            -float_panel $itk_option(-float_panel) \
            -newimagecmd [code $this cleared] \
            -temporary $itk_option(-temporary) \
            -grid_command [code $this maybe_draw_grid_] \
            -with_warp 1 \
            -panel_layout $itk_option(-panel_layout) \
            -regioncommand [code $this select_region] \
            -component $itk_option(-component) \
            -min_scale $itk_option(-min_scale) \
            -max_scale $itk_option(-max_scale) \
            -pickobjectorient $itk_option(-pickobjectorient)
      }

      #  Keep a list of SkyCat/GAIA instances.
      global ::skycat_images
      lappend skycat_images $itk_component(image)
   }

   #  Add the menubar at the top of the window (override this so we can
   #  Add keyboard bindings).
   public method add_menubar {} {
      # menu bar
      TopLevelWidget::add_menubar

      # File menu
      set m [add_menubutton File]

      configure_menubutton File -underline 0
      add_menuitem $m command "Open..." \
         {Open and display an image} \
         -command [code $image_ open] \
         -accelerator {Control-o}
      bind $w_  <Control-o> [code $image_ open]


      add_menuitem $m command "Save as..." \
         {Save current image to a file} \
         -command [code $image_ save_as] \
         -accelerator {Control-v}
      bind $w_ <Control-v> [code $image_ save_as]


      add_menuitem $m command "Print..." \
         {Print the current image and graphics to a file or printer} \
         -command [code $image_ print] \
         -accelerator {Control-i}
      bind $w_ <Control-i> [code $image_ print]

      $m add separator

      add_menuitem $m command "New Window" \
         {Create a new main window} \
         -command [code $this clone] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone]

      add_menuitem $m command "Delete Window" \
         {Close this window} \
         -command [code $this remove 0] \
         -accelerator {Control-d}
      bind $w_ <Control-d> [code $this remove 0]

      $m add separator

      add_menuitem $m command "Exit"\
         {Exit the program} \
         -command "destroy ." \
         -accelerator {Control-x}
      bind $w_ <Control-x> "destroy ."

      # View menu
      set m [add_menubutton View]
      configure_menubutton View -underline 0
      add_menuitem $m command "Colors..." \
         {Change the display colours} \
         -command [code $image_ set_colors] \
         -accelerator {Control-u}
      bind $w_ <Control-u> [code $image_ set_colors]

      add_menuitem $m command "Cut Levels..." \
         {Change the maximum and minimum display levels} \
         -command [code $image_ set_cut_levels] \
         -accelerator {Control-l}
      bind $w_ <Control-l> [code $image_ set_cut_levels]

      add_menuitem $m command "Slice..." \
         {Display the data values along a line} \
         -command [code $image_ spectrum 1] \
         -accelerator {Control-s}
      bind $w_ <Control-s> [code $image_ spectrum 0]

      add_menuitem $m command "Fits header..." \
         {Display the image FITS headers} \
         -command [code $image_ view_fits_header]

      add_menuitem $m command "Pick Object..." \
         {Select an object or star in the image and display statistics} \
         -command [code $image_ pick_dialog]

      add_menuitem $m cascade "Pixel Table..." \
         {Display a table of values about the cursor position} \
         -menu [menu $m.pix]
      $m.pix add command -label "3x3" -command [code $image_ pixel_table 3 3]
      $m.pix add command -label "5x5" -command [code $image_ pixel_table 5 5]
      $m.pix add command -label "7x7" -command [code $image_ pixel_table 7 7]
      $m.pix add command -label "9x9" -command [code $image_ pixel_table 9 9]

      add_menuitem $m cascade "Magnification" \
         {Change the image magnification} \
         -menu [menu $m.mag]
      after idle [code $itk_component(image).panel.info.trans fill_mag_menu $m.mag]

      add_menuitem $m cascade "Image background" \
         {Change the background colour of the main window} \
         -menu [menu $m.back]
      foreach colour $colours_ {
         $m.back add radiobutton \
            -background $colour \
            -variable $w_.colour \
            -value $colour \
            -label {    } \
            -command [code $this set_background_ $colour]
      }

      add_menuitem $m cascade "Blank pixel color" \
         {Change the colour of blank pixels} \
         -menu [menu $m.blank]
      foreach colour $colours_ {
         $m.blank add radiobutton \
            -background $colour \
            -variable $w_.colour \
            -value $colour \
            -label {    } \
            -command [code $this set_blankcolour_ $colour]
      }

      $m add separator

      if { ! $itk_option(-float_panel) } {
         add_menuitem $m checkbutton "Hide Control Panel" \
            {Toggle the visibility of the upper control panel} \
            -variable $w_.hide_control_panel -onvalue 1 -offvalue 0 \
            -command [code $image_ hide_control_panel $w_.hide_control_panel]
      }

      add_menuitem $m checkbutton "Hide Popup Windows" \
         {Toggle the visibility of the popup windows} \
         -variable $w_.hide_windows -onvalue 1 -offvalue 0 \
         -command [code $this hide_windows $w_.hide_windows]

      # Graphics menu
      set m [add_menubutton Graphics]

      add_menuitem $m command "Toolbox..." \
         {Display the graphics toolbox for drawing on the image} \
         -command [code $image_ show_toolbox]

      $m add separator

      [$image_ component draw] add_menuitems $m
      $m add separator

      add_menuitem $m checkbutton "Hide Graphics" \
         {Toggle the visibility of the image line graphics} \
         -variable $w_.hide_graphics -onvalue 1 -offvalue 0 \
         -command [code $image_ hide_graphics $w_.hide_graphics]

	# Real-time menu
	set m [add_menubutton "Real-time"]

	add_menuitem $m command "Attach Camera" \
	    {Attach the real-time camera - start receiving images} \
	    -command [code $this attach_camera]

	add_menuitem $m command "Detach Camera" \
	    {Detach the real-time camera - stop receiving images} \
	    -command [code $this detach_camera]

	add_menuitem $m command "Set Camera..." \
	    {Set the real-time camera name} \
	    -command [code $this set_camera]

	$m add separator

	add_menuitem $m cascade "Rapid Frame" \
	    {Create a rapid frame by interactively drawing a rectangle on the image} \
	    -menu [menu $m.rapid]

	$m.rapid add command -label "In Canvas" \
	    -command [code $image_ rapid_frame 0]
	$m.rapid add command -label "In Separate Window" \
	    -command [code $image_ rapid_frame 1]

	$m add separator

	global ::$w_.preview_mode
	set $w_.preview_mode 0

	add_menuitem $m checkbutton "Preview Mode" \
	    {Preview mode: copy the real-time image from shared memory to local memory} \
	    -variable $w_.preview_mode -onvalue 1 -offvalue 0 \
	    -command [code $image_ preview $w_.preview_mode]
   }

   #  Add a menubutton with the GAIA options.
   public method add_gaia_menu {} {

      set m [add_menubutton Image-Analysis]
      configure_menubutton Image-Analysis -underline 0
      add_short_help $itk_component(menubar).image-analysis \
         {Image analysis menu: do astronomy with image}

      add_menuitem $m cascade "Aperture photometry" \
         {Perform aperture photometry on image} \
         -menu [menu $m.photom]

      add_menuitem $m.photom command "Results in magnitudes..." \
         {Display aperture photometry toolbox (results in magnitudes)} \
         -command [code $this make_toolbox magphotom] \
         -accelerator {Control-m}
      bind $w_ <Control-m> [code $this make_toolbox magphotom]

      add_menuitem $m.photom command "Results in data counts..." \
         {Display aperture photometry toolbox (results in image data units)} \
         -command [code $this make_toolbox countphotom] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this make_toolbox countphotom]

      add_menuitem $m command "Image regions..." \
         {Perform operations on regions of image} \
         -command [code $this make_toolbox ard] \
         -accelerator {Control-r}
      bind $w_ <Control-r> [code $this make_toolbox ard]

      add_menuitem $m command "Patch image..." \
         {Realistically replace parts of image} \
         -command [code $this make_toolbox patch] \
         -accelerator {Control-p}
      bind $w_ <Control-p> [code $this make_toolbox patch]

      add_menuitem $m command "Blink images..." \
         {Blink compare all the displayed images} \
         -command [code $this make_toolbox blink] \
         -accelerator {Control-b}
      bind $w_ <Control-b> [code $this make_toolbox blink]

      add_menuitem $m command "Overlay axes grid..." \
         {Draw axes over image } \
         -command [code $this make_toolbox astgrid] \
         -accelerator {Control-t}
      bind $w_ <Control-t> [code $this make_toolbox astgrid]

      add_menuitem $m cascade "Astrometry calibration" \
         {Create and manipulate astrometry information} \
         -menu [menu $m.astrom]

      add_menuitem $m.astrom command "Fit to star positions..." \
         {Create a WCS for image using reference positions} \
         -command [code $this make_toolbox astreference] \
         -accelerator {Control-f}
      bind $w_ <Control-f> [code $this make_toolbox astreference]

      add_menuitem $m.astrom command "Tweak an existing calibration..." \
         {Use linear transforms to refine the WCS associated with this image} \
         -command [code $this make_toolbox astrefine] \
         -accelerator {Control-z}
      bind $w_ <Control-z> [code $this make_toolbox astrefine]

      add_menuitem $m.astrom command "Copy from another image..." \
         {Copy a WCS from another image} \
         -command [code $this make_toolbox astcopy] \
         -accelerator {Control-y}
      bind $w_ <Control-y> [code $this make_toolbox astcopy]

      add_menuitem $m.astrom command "Type in known calibration..." \
         {Define a WCS for image using FITS-like description} \
         -command [code $this make_toolbox astdefine] \
         -accelerator {Control-w}
      bind $w_ <Control-w> [code $this make_toolbox astdefine]

      add_menuitem $m command "Celestial coordinates...  " \
         {Change the image celestial coordinate system} \
	 -command [code $this make_toolbox astsystem] \
	 -accelerator {Control-e}
      bind $w_ <Control-e> [code $this make_toolbox astsystem]

      add_menuitem $m command "Object detection...  " \
         {Automatically detect and parameterize objects...} \
	 -command [code $this make_toolbox sextractor] \
	 -accelerator {Control-j}
      bind $w_ <Control-j> [code $this make_toolbox sextractor]

      if { $itk_option(-demo_mode) } {
	 add_menuitem $m command "Demonstration mode..." \
	       {See a demonstration of GAIA (needs an empty directory)} \
	       -command [code $this make_toolbox demo]
      }
   }

   #  Make or clone a GAIA toolbox.
   public method make_toolbox {type {clone 0} } {
      #  Do nothing if no image is displayed.
      if { [$image_ cget -file] != "" || $type == "demo" } {
         set basename $type
         if { $clone } {
            #  Request to create a clone (i.e. another) toolbox. Make
            #  extended name for this.
            set basename "$basename[incr tool_clones_]"
         }

         #  If the window exists then just raise it.
         if { [info exists itk_component($basename) ] &&
              [winfo exists $itk_component($basename) ] } {
            wm deiconify $itk_component($basename)
            raise $itk_component($basename)
         } else {
            busy {
               make_${type}_toolbox $basename $clone
            }
         }
      }
   }
   
   #  Make a magnitude photometry toolbox.
   public method make_magphotom_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarPhotom .\#auto 1 \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox magphotom 1]
      }
   }

   #  Make a counts photometry toolbox.
   public method make_countphotom_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarPhotom .\#auto 0 \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox countphotom 1]
      }
   }

   #  Make an ARD toolbox.
   public method make_ard_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarArd .\#auto \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -gaia $w_ \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox ard 1]
      }
   }

   #  Make an AST grid toolbox.
   public method make_astgrid_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarAstGrid .\#auto \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox astgrid 1] \
            -really_die $cloned
      }
   }

   #  Make an AST reference WCS toolbox.
   public method make_astreference_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarAstReference .\#auto \
            -image $image_ \
            -rtdimage [$image_ get_image] \
            -canvas [$image_ get_canvas] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this maybe_draw_grid_ 1] \
            -clone_cmd [code $this make_toolbox astreference 1] \
            -really_die $cloned
      }
   }

   #  Make an AST define WCS toolbox.
   public method make_astdefine_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarAstDefine .\#auto \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this maybe_draw_grid_ 1] \
            -clone_cmd [code $this make_toolbox astdefine 1] \
            -really_die $cloned
      }
   }

   #  Make an AST copy WCS toolbox.
   public method make_astcopy_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarAstCopy .\#auto \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -filter_types $itk_option(-file_types) \
            -notify_cmd [code $this maybe_draw_grid_ 1] \
            -clone_cmd [code $this make_toolbox astcopy 1] \
            -really_die $cloned
      }
   }

   #  Make an AST refine WCS toolbox or make it visible.
   public method make_astrefine_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarAstRefine .\#auto \
            -image $image_ \
            -rtdimage [$image_ get_image] \
            -canvas [$image_ get_canvas] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this maybe_draw_grid_ 1] \
            -clone_cmd [code $this make_toolbox astrefine 1] \
            -really_die $cloned
      }
   }

   #  Make an AST set celestial coordinates system toolbox.
   public method make_astsystem_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarAstSystem .\#auto \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -notify_cmd [code $this maybe_draw_grid_ 1] \
            -clone_cmd [code $this make_toolbox astsystem 1] \
            -really_die $cloned
      }
   }

   #  When image is flipped etc. we may want to redraw the grid, if
   #  it is visible (that is not withdrawn). If auto is set 1 then
   #  the grid is only redrawn if automatic redraws are on.
   protected method maybe_draw_grid_ { {auto 0} } {
      if { [info exists itk_component(astgrid) ] &&
           [winfo exists $itk_component(astgrid) ] } {

         #  Check that window isn't withdrawn
         if { [wm state $itk_component(astgrid)] != "withdrawn" } {
            $itk_component(astgrid) draw_grid 0 $auto
         }
      }
   }

   #  Make a patch toolbox.
   public method make_patch_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarPatch .\#auto \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox patch 1]
      }
   }

   #  Blink any displayed images.
   public method make_blink_toolbox {name {cloned 0}} {
      if { $clone_cnt > 0 } {
         itk_component add $name {
            StarBlink .\#auto \
               -transient $itk_option(-transient_tools) \
               -number $clone_ \
               -clone_cmd [code $this make_toolbox blink 1]
         }
      } else {
         error_dialog "Not enough images are displayed to blink."
      }
   }

   #  Make a sextractor toolbox.
   public method make_sextractor_toolbox {name {cloned 0}} {
      itk_component add $name {
         GaiaSextractor .\#auto \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -rtdimage [$image_ get_image]\
            -image $image_ \
            -filter_types $itk_option(-file_types) \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox sextractor 1]
      }
   }


   #  Start the demonstration toolbox.
   public method make_demo_toolbox {name {cloned 0}} {
      itk_component add $name {
         StarDemo .\#auto \
            -gaiamain $w_ \
            -rtdimage [$image_ get_image] \
            -gaiactrl $image_ \
            -canvasdraw [$image_ component draw] \
            -canvas [$image_ get_canvas] \
            -transient $itk_option(-transient_tools) \
            -number $clone_ \
            -clone_cmd [code $this make_toolbox demo 1]
      }
   }

   #  Make the "Filters" menu.
   public method make_filters_menu {} {
      StarAppFilter \#auto $w_
   }

   #  Open a new file without a filebrowser, or return the name of the
   #  displayed file. Always use an absolute name (for matching etc.)
   public method open {args} {
      if {"$args" != ""} {
         set file [lindex $args 0]
         if {[file isfile $file]} {
            if { [catch {set here [pwd]}] } {
               set here ""
            }
            if { ! [string match {/*} $file] } {
               set file "$here/[file tail $file]"
            }
            configure -file $file

            #  Make sure image exists before using it.
            $image_ configure -file $file

            #  Lower the image on the canvas so that any existing
            #  graphics are revealed.
            [$image_ get_canvas] lower [$image_ get_image]
         } else {
            error_dialog "There is no file named '$file'" $w_
         }
      } else {
         return $itk_option(-file)
      }
   }

   #  Make a new main window, named either the next in sequence
   #  or using a given name.
   public method clone {{clone ""} {file ""} args} {

      #  If given the file replaces the one in the command-line args or
      #  is added to the list.
      if { $file != "" } {
         global ::argv ::argc
         set index [lsearch -exact $argv "-file"]
         if { $index == -1 } {
            lappend argv "-file"
            lappend argv "$file"
            incr argc 2
         } else {
            incr index
            set argv [lreplace $argv $index $index $file]
         }
      }

      #  If clone exists just return the name and display the image.
      if { "$clone" != "" } {
         if { [winfo exists .rtd$clone] } {
            if { $file != "" } {
               .rtd$clone open $file
            }
            if { $args != "" } {
               eval configure $args
            }
            return .rtd$clone
         }
         if { $clone > $clone_max_ } {
            set clone_max_ $clone
         }
      } else {
         #  New clone required.
         set clone [incr clone_max_]
      }

      incr clone_cnt

      #  Do not use TopLevelWidget::start as this blocks with a tkwait
      #  which means we can never detect when the clone is complete.
      # after 0 [code TopLevelWidget::start Gaia "-file" {} .rtd$clone]
      global argv
      eval Gaia ::.rtd$clone $argv $args

      #  Wait until clone window appears.
      tkwait visibility .rtd$clone
      return .rtd$clone
   }

   #  Image has been cleared so reset any toolboxes that require it
   #  (note most just have canvas objects which are deleted when
   #  a new image is drawn and do not require any other information).
   public method cleared {} {
      if { [info exists itk_component(astgrid) ] } {
         if { [winfo exists $itk_component(astgrid) ] } {
            $itk_component(astgrid) remove_grid
         }
      }
      if { [info exists itk_component(astsystem) ] } {
         if { [winfo exists $itk_component(astsystem) ] } {
            $itk_component(astsystem) image_changed
         }
      }
   }

   #  Return the name of the GaiaImageCtrl so that other external
   #  routines may talk to it.
   public method get_image {} { return $image_ }

   #  Set the colour of the main canvas background.
   protected method set_background_ {colour} {
      [$image_ get_canvas] configure -background $colour
   }

   #  Set the colour of the any blank pixels.
   protected method set_blankcolour_ {colour} {
      [$image_ get_image] blankcolor $colour
   }

   #  Start a process to load the ESO config file. Need this in case
   #  local version becomes horribly out of date.
   public method load_eso_config {} {

      #  Attempt to get the default config file using a batch process
      #  to avoid nasty blocking.
      Batch $w_.bg_proc -command [code $this loaded_eso_config_]
      blt::busy hold $w_
      $w_.bg_proc bg_eval [code $this get_eso_config_]

      #  Query times out after 10 seconds.
      set after_id_ [after 10000 [code $this loaded_eso_config_ 1 "timed out"]]
   }

   #  Retrieve the ESO config file, returning its content as the result.
   protected method get_eso_config_ {} {
      return [[$image_ get_image] urlget $itk_option(-eso_config_file)]
   }

   #  Attempt to load ESO config file is completed. If succeeded in
   #  contact them overwrite the local ~/.skycat/skycat.cfg file and
   #  force a local reload.
   protected method loaded_eso_config_ {status msg} {
      blt::busy release $w_
      if { $status } {
         error_dialog "Failed to get ESO config file: $msg"
      } else {

         #  msg is file contents, need to make this the local file.
         global ::env
         set file $env(HOME)/.skycat/skycat.cfg
         if {[file exists $file]} {
            if {[catch {::file rename -force $file $file.BAK} error]} {
               error_dialog $error
               return
            }
         }
         set fd [::open $file w]
         puts $fd $msg
         ::close $fd

         #  Finally force this to be loaded.
         AstroCat::reload_config_file $w_
      }
      delete object $w_.bg_proc
      if { $after_id_ != {} } {
         catch [after cancel $after_id_]
         set after_id_ {}
      }
   }

   # -- public variables (also program options) --

   #  Float the control panel (better real estate control on small
   #  displays).
   itk_option define -float_panel float_panel Float_panel 0

   #  Mark displayed image as temporary, these are deleted on exit
   #  (after a request to save it is made), try to disable options
   #  database configuring this.
   itk_option define -temporary teMpoRaRy TeMpoRaRy 0 {
      if { [info exists image_] } {
         if { [winfo exists $image_] } {
            $image_ configure -temporary $itk_option(-temporary)
         }
      }
   }

   #  Whether toolboxes are transient (iconize with main window).
   itk_option define -transient_tools transient_tools Transient_Tools 0

   #  The known file types.
   itk_option define -file_types file_types File_Types {{any *}}

   #  Whether to reveal the GAIA menu or not.
   itk_option define -gaia gaia Gaia 1

   #  Whether to reveal the filters menu or not.
   itk_option define -filters filters Filters 0

   #  Redefine scrollbars to be true.
   itk_option define -scrollbars scrollbars ScrollBars 1

   #  The NDF component to display.
   itk_option define -component component Component data

   #  Switch on demo mode (this makes an additional class available
   #  for running GAIA and making it show off/test some of its
   #  capabilities).
   itk_option define -demo_mode demo_mode Demo_Mode 1

   #  Name of ESO catalogue config file.
   itk_option define -eso_config_file eso_config_file Eso_config_file \
      {http://archive.eso.org/skycat/skycat2.0.cfg}

   #  Redefine panel_layout to GAIA default
   itk_option define -panel_layout panel_layout Panel_layout reverse

   # -- Protected variables --

   #  Clone number of this window.
   protected variable clone_ 0

   #  Initialization progress count.
   protected variable sofar_ 0

   #  Initialization windows.
   protected variable Init_
   protected variable Progress_

   #  Colours of the main background.
   protected variable colours_ {
      white
      grey90 grey80 grey70 grey60 grey50 grey40 grey30 grey20 grey10
      black
      red green blue cyan magenta yellow
   }

   #  Id of after command for reading ESO config file.
   protected variable after_id_ {}

   #  Number for creating toolbox clones.
   protected variable tool_clones_ 0

   # -- Common variables --

   # number of clones of this window
   common clone_cnt 0

   # maximum clone number so far
   common clone_max_ 0
}

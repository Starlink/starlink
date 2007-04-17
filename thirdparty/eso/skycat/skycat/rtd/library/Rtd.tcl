# E.S.O. - VLT project
# "@(#) $Id: Rtd.tcl,v 1.1.1.1 2006/01/12 16:38:15 abrighto Exp $"
#
# Rtd.tcl - real-time image display application class
# See man page Rtd(n) for a complete description.
#
# who           when       what
# --------     ---------   ----------------------------------------------
# A.Brighton   11/10/95    created
#              23/06/96    added real-time test (../test/tRtd), use -debug 1 option
# P.Biereichel 30/06/97    attach to camera in method rapid_frame_command
#                          Added itk_option(-with_warp)
#                          Default itk_option(-disp_image_icon) set to 0
# A.Brighton   22/03/98    Added -float_panel option to allow info panel to be
#                          in a popup window (default is off, no change),
#                          added -panel_layout option to allow changing the
#                          order of the zoom and pan windows in the layout
#                          (default is the same as before, no change).
# P.Biereichel 22/03/99    Added bias subtraction to real-time menu
# P.Biereichel 04/08/99    Added itk_options -attach, -geometry
#                          Added rtd_status and rtd_about
# P.Biereichel 25/05/00    Added auto scale
# P.Biereichel 20/04/04    Added $w_ to input_dialog in method set_camera

itk::usual Rtd {}

# This class defines the top level window for the rtd (real-time image
# display) application.  The window contains a menubar with rtd related
# items, an RtdImageCtrl widget for displaying the image, and related
# info and a short help window.
# 
# The easiest way to use this class is via the "start" method (inherited
# from TopLevelWidget).  This creates an instance of this class and
# passes any command line options as public variables to the class and
# waits for window to be exited. Alternatively, you can create the
# instance in the usual way for itcl classes and withdraw the main
# window ".", if it is not being used.
# 
# One global variable is assumed to have been defined:
# 
# rtd_library - dir containing Rtd Tcl sources.

itcl::class rtd::Rtd {
    inherit util::TopLevelWidget

    # constructor: create a toplevel window

    constructor {args} {
	eval itk_initialize $args
    }

    destructor {
	catch {kill $rapid_pid_}
    }
    
    # This method is called after the options have been evaluated.

    protected method init {} {
	global ::argv0 ::errorInfo ::rtd_version

	# set/get X defaults first time through - can be overridden in 
	# subclass and/or in user's .Xdefaults file
	if {$itk_option(-number) == 1} {
	    setXdefaults
	}
	wm protocol $w_ WM_DELETE_WINDOW "[code $this close]"

	feedback "initializing user interface..."
	if {"$itk_option(-number)" == "1"} {
	    wm title $w_ "Rtd - Real-Time Display, version $rtd_version"
	} else {
	    wm title $w_ "Rtd - Real-Time Display, version $rtd_version ($itk_option(-number))"
	}
	wm iconname $w_ Rtd
	
	# set rtd camera if not set on command line, default to environment var
	global ::env
	if {"$itk_option(-camera)" == ""} {
	    if {[info exists env(RTD_CAMERA)]} {
		config -camera $env(RTD_CAMERA)
	    } else {
		config -camera RTDSIMULATOR
	    }
	}

	feedback "making image window..."
	# create the rtd image widget and exit on errors, such as no more 
	# colors, no more memory...
	if {[catch {make_rtdimage} msg]} {
	    puts $errorInfo
	    exit 1
	}

	# display the image also as an icon ?
	if {$itk_option(-disp_image_icon)} {
	    feedback "icon..."
	    # optional RtdImage image icon
	    itk_component add icon {
		rtd::RtdImageIcon $w_.icon \
			-image $itk_component(image) \
			-usexshm $itk_option(-usexshm) \
			-usexsync $itk_option(-usexsync) \
			-verbose $itk_option(-verbose) \
			-subsample $itk_option(-subsample) \
			-center 0
	    }
	    wm iconwindow $w_ $w_.icon
	}

	feedback "menubar..."
	# add the menubar and short help
	add_menubar
	pack $itk_component(image) -fill both -expand 1
    	make_short_help
	
	# make a message if we are using a private colormap
	if {[$image_ cmap isprivate]} {
	    set prog [file rootname [file tail $argv0]]
	    catch {
		puts "\n$prog: unable to allocate enough colors for image display \
			in the default colormap - using a private colormap. If this \
			causes any problems with color flashing, try exiting other \
			color intensive applications (such as netscape) first\n"
	    }
	}
	if {$attach_} {
	    after idle [code $this attach_camera]
	} else {
	    after idle "$itk_component(image) updateCameraStatus $itk_option(-camera)"
	}
    }
    
    # this method can be redefined in a subclass to get feedback during
    # startup

    public method feedback {msg} {
    }


    # set default X resources for colors and fonts, and set some default key
    # bindings. This is done in a method so that it can be overridden by a subclass. 
    # These are built-in defaults that the user can also override in the ~/.Xdefaults
    # file.

    public method setXdefaults {} {
	rtd::setXdefaults
    }


    # create the rtd image widget

    protected method make_rtdimage {} {
	set image_ $w_.image
	# RtdImageCtrl(n) widget containing image and control panel.
	itk_component add image {
	    rtd::RtdImageCtrl $image_ \
		    -file $itk_option(-file) \
		    -usexshm $itk_option(-usexshm) \
		    -usexsync $itk_option(-usexsync) \
		    -verbose $itk_option(-verbose) \
		    -shm_header $itk_option(-shm_header) \
		    -shm_data $itk_option(-shm_data) \
		    -min_colors $itk_option(-min_colors) \
		    -max_colors $itk_option(-max_colors) \
		    -drag_scroll $itk_option(-drag_scroll) \
		    -scrollbars $itk_option(-scrollbars) \
		    -subsample $itk_option(-subsample) \
		    -sampmethod $itk_option(-sampmethod) \
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
		    -color_scale $itk_option(-color_scale) \
		    -default_cmap $itk_option(-default_cmap) \
		    -default_itt $itk_option(-default_itt) \
		    -with_colorramp $itk_option(-with_colorramp) \
		    -rapid_frame_command [code $this rapid_frame_command] \
		    -feedback [code $this feedback] \
		    -port $itk_option(-port) \
		    -shorthelpwin $this \
		-debug [expr {$itk_option(-debug) != 0}] \
		    -camera $itk_option(-camera) \
		    -float_panel $itk_option(-float_panel) \
		    -panel_layout $itk_option(-panel_layout) \
		    -panel_orient $itk_option(-panel_orient) \
		    -with_grid $itk_option(-with_grid) \
		    -with_warp $itk_option(-with_warp) \
		    -min_scale $itk_option(-min_scale) \
		    -max_scale $itk_option(-max_scale) \
		    -pickobjectorient $itk_option(-pickobjectorient) \
		    -updatePick $itk_option(-updatePick) \
		    -xscale $itk_option(-xscale) \
		    -yscale $itk_option(-yscale) \
		    -image_directory $itk_option(-image_directory)
	}
	# If this is a cloned widget, use a different image name
	if {[cget -number] > 1} {
	    $itk_component(image) config -name \
		    "[$itk_component(image) cget -name][cget -number]"
	}
    }


    # add the menubar at the top of the window

    protected method add_menubar {} {
	# menu bar
	TopLevelWidget::add_menubar

	# add menubuttons and menus
	add_file_menu
	add_view_menu
	add_graphics_menu
	add_realtime_menu
	add_help_menu
    }


    # add the File menubutton and menu

    protected method add_file_menu {} {
	set m [add_menubutton File]

	add_short_help $itk_component(menubar).file \
		{File menu: load, save, print, clear image, new window, exit application}

	add_menuitem $m command "Open..." \
		{Open and display an image file} \
		-command [code $image_ open] \
		-accelerator "Control-o"

	add_menuitem $m command "Reopen" \
		{Reload the image display after the image has changed on disk} \
		-command [code $image_ reopen] \
		-accelerator "Control-v"

	add_menuitem $m command "Save as..." \
		{Save the current image to a file} \
		-command [code $image_ save_as] \
		-accelerator "Control-s"

	add_menuitem $m command "Save region as..." \
		{Save a section of the current image to a file} \
		-command [code $image_ save_region_as] \
		-accelerator "Control-S"

	add_menuitem $m command "Print..." \
		{Print the current image to a file or printer} \
		-command [code $image_ print] \
		-accelerator "Control-P"

	add_menuitem $m command Clear \
		{Clear the image display} \
		-command [code $this clear]

	$m add separator

        add_menuitem $m command "Bias Image..." \
		{Control subtraction of a bias image} \
		-command [code $image_ set_bias] \
		-accelerator "Control-b"

        $m add separator

	add_menuitem $m command "New Window" \
		{Display up a new main window} \
		-command [code $this clone] \
		-accelerator "Control-n"

	add_menuitem $m command "Close" \
		{Close the main window and exit if there are no more windows} \
		-command [code $this close]

	$m add separator

	add_menuitem $m command Exit \
		{Exit the application} \
		-command [code $this quit] \
		-accelerator "Control-q"
    }


    # add the VIew menubutton and menu

    protected method add_view_menu {} {
	
	set m [add_menubutton View]

	add_short_help $itk_component(menubar).view \
		{View menu: manipulate colors, cut levels, pixel info}

	add_menuitem $m command "Colors..." \
		{Display a window for manipulating the image colormap} \
		-command [code $image_ set_colors] \
		-accelerator "Control-c"

	add_menuitem $m command "Cut Levels..." \
		{Display a window for manipulating the image cut levels} \
		-command [code $image_ component info cut_level_dialog] \
		-accelerator "Control-l"

	add_menuitem $m command "Cuts..." \
		{Display a graph of pixel values along a line drawn interactively over the image} \
		-command [code $image_ spectrum] \
		-accelerator "Control-u"

	add_menuitem $m command "Pick Object..." \
		{Select an object or star in the image and display statistics} \
		-command [code $image_ pick_dialog] \
		-accelerator "Control-p"

	add_menuitem $m command "Fits header..." \
		{Display the FITS header for the current image} \
		-command [code $image_ view_fits_hdu_header] \
		-accelerator "Control-f"

	# XXX note: the WCS Info impl. still needs work to display the 
	# correct values (setting values is OK)
	# pbiereic: Let's wait for an SPR
	add_menuitem $m command "WCS Info..." \
		{Set/Display World Coordinate information for the current image} \
		-command [code $image_ wcs_info_dialog]

	add_menuitem $m cascade "Pixel Table..." \
		{Display a table of pixel values surrounding the mouse cursor} \
		-menu [menu $m.pix]
	$m.pix add command -label "3x3" -command [code $image_ pixel_table 3 3]
	$m.pix add command -label "5x5" -command [code $image_ pixel_table 5 5]
	$m.pix add command -label "7x7" -command [code $image_ pixel_table 7 7]
	$m.pix add command -label "9x9" -command [code $image_ pixel_table 9 9]

	add_menuitem $m cascade "Magnification" \
		{Set the magnification factor of the image display} \
		-menu [menu $m.mag]
	after idle [code $itk_component(image) component info component trans fill_mag_menu $m.mag]

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

	if {0} {
	    add_menuitem $m checkbutton "Show Grid" \
		    {Toggle the visibility of the image ra,dec grid} \
		    -variable $w_.image.grid -onvalue 1 -offvalue 0 \
		    -command [code $image_ show_grid $w_.image.grid]
	}

        $m add separator
        
        add_menuitem $m checkbutton "Auto scale" \
                {Scale the image to the max. visible size} \
                -variable $w_.autoscale -onvalue 1 -offvalue 0 \
                -command [code $image_ autoscale $w_.autoscale]

	$m add separator

	add_menuitem $m command "Select FITS HDU..." \
	    {Display the available FITS HDUs (header/data units) and select the current HDU} \
	    -command [code $image_ display_fits_hdus]
    }


    # add the Graphics menubutton and menu

    protected method add_graphics_menu {} {
	set m [add_menubutton Graphics]

	add_short_help $itk_component(menubar).graphics \
		{Graphics menu: display graphics window or set graphics options}

	add_menuitem $m command "Toolbox..." \
		{Display the graphics toolbox for drawing on the image} \
		-command [code $image_ show_toolbox] \
		-accelerator "Control-t"

	$m add separator

	[$image_ component draw] add_menuitems $m
	$m add separator

	add_menuitem $m checkbutton "Hide Graphics" \
		{Toggle the visibility of the image line graphics} \
		-variable $w_.hide_graphics -onvalue 1 -offvalue 0 \
		-command [code $image_ hide_graphics $w_.hide_graphics]
    }


    # add the Real-time menubutton and menu

    protected method add_realtime_menu {} {
	set m [add_menubutton "Real-time"]

	add_short_help $itk_component(menubar).real-time \
		{Real-time menu: real-time display commands. rapid frame}

	add_menuitem $m command "Attach Camera" \
		{Attach the real-time camera - start receiving images} \
		-command [code $this attach_camera] \
		-accelerator "Control-a"

	add_menuitem $m command "Detach Camera" \
		{Detach the real-time camera - stop receiving images} \
		-command [code $this detach_camera] \
		-accelerator "Control-d"

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
	$m.rapid add command -label "Delete Rapid Frame" \
		-command [code $image_ delete_rapid_frame]

	$m add separator

	global ::$w_.preview_mode
	set $w_.preview_mode 0

	add_menuitem $m checkbutton "Preview Mode" \
		{Preview mode: copy the real-time image from shared memory to local memory} \
		-variable $w_.preview_mode -onvalue 1 -offvalue 0 \
		-command [code $image_ preview $w_.preview_mode]

        $m add separator

        add_menuitem $m command "Record/Playback Images..." \
		{Record and playback real time images} \
		-command [code $this record]

	if {$itk_option(-with_perftest)} {
	    $m add separator
	    
	    add_menuitem $m command "Performance..." \
		    {Performance test: enable interactive performance test parameters} \
		    -command [code $image_ perftest]
	}
    }

    # add a menubutton with help items

    protected method add_help_menu {} {
        set m [add_menubutton "Help" {} right]

        add_menuitem $m command "Status..." \
		{Display a window with status information} \
		-command [code $this rtd_status]

        add_menuitem $m command "About Rtd..." \
		{Display a window with information about this Rtd version} \
		-command [code $this rtd_about]

        add_short_help $itk_component(menubar).help \
		{Help menu: display information about this application}
    }

    # make a new main window

    public method clone {} {
	global ::rtd_usage
	# use the -noop option to avoid reloading the main image (part of $argv list)
	after 0 [code util::TopLevelWidget::start Rtd "-noop" "$rtd_usage"]
    }


    # close the application

    public method close {} {
	delete object $this
    }

    # quit the application

    public method quit {} {
	delete object $this
	after idle exit
    }

    # attach the current camera

    public method attach_camera {} {
	# debug: for testing
	if {$itk_option(-debug) == 1} {
	    utilReUseWidget rtd::tRtd $w_.tRtd \
		    -camera $itk_option(-camera) \
		    -rtdimage $image_ \
		    -testprog [cget -testprog] \
		    -interval $itk_option(-interval)
	}
	$image_ attach_camera $itk_option(-camera)
	$itk_component(image) updateCameraStatus $itk_option(-camera)
    }
	
    # detach the current camera

    public method detach_camera {} {
	if {$itk_option(-debug) && [winfo exists $w_.tRtd]} {
	    $w_.tRtd close
	} else {
	    $image_ detach_camera
	}
	$itk_component(image) updateCameraStatus $itk_option(-camera)
    }
    

    # popup a window to query for new camera
    
    public method set_camera {} {
	set sts [[$image_ get_image] camera attach]
	if {$sts} {
	    set s "\"$itk_option(-camera)\" is attached"
	} else {
	    set s "\"$itk_option(-camera)\" is detached"
	}
        set cam [input_dialog "The current camera is: \"$itk_option(-camera)\"\n$s\n\
		Please enter a new camera name:" $w_]
        if { $cam != "" } {
            configure -camera $cam
	    if {$sts} {
		attach_camera
	    }
        }
    }

    # Methods for the playing and recording of images.

    public method record {} {
        $image_ record $itk_option(-camera)
    }
    
    # add the short help window and add some help texts for the menu buttons

    protected method make_short_help {} {
	TopLevelWidget::make_short_help
    }

    
    # Called for "Clear" menu item. Clear the image and delete all graphics

    public method clear {} {
	$image_ clear
	
	# delete any remaining graphics
	set canvas [$image_ get_canvas]
	foreach tag [$canvas find all] {
	    if {"[$canvas type $tag]" != "image"} {
		$canvas delete $tag
	    }
	}
    }


    # This method is called when the user creates, moves, resizes
    # or deletes a rapid frame. 
    #
    # The args are:
    #
    #  frameId = unique rapid frame id for use with rtdServer
    #
    #  name = unique name for the frame
    #
    #  op  = {move,resize or delete},
    #
    #  x, y = coords of upper left corner of frame in image 
    #
    #  w, h = dimensions of frame.

    protected method rapid_frame_command {frameId name op x y w h} {
	if {! $itk_option(-debug)} { return }

	catch {kill $rapid_pid_}
	if {"$op" == "delete"} { return }

	set im [$image_ get_image] 
	$im convert dist $x $y canvas ix iy image
	$im convert dist $w $h canvas iw ih image
	if {[catch {set rapid_pid_ [exec $itk_option(-testprog) \
		-v $itk_option(-verbose) \
		-t $itk_option(-interval) \
		-x $ix -y $iy -w $iw -h $ih -f $frameId &]} msg]} {
	    error_dialog "error starting $itk_option(-testprog): $msg"
	} else {
	    $image_ attach_camera $itk_option(-camera)
	}
    }

    
    # display a popup window with status information
    
    public method rtd_status {} {
	set t1 "Rtd [package versions Rtd], Status"
	set s1 "Object:\t\t[[$image_ get_image] object]"
	set s2 "Camera name:\t[cget -camera]"
	if {[[$image_ get_image] camera attach]} {
	    set s3 "Camera:\t\tattached"
	} else {
	    set s3 "Camera:\t\tdetached"
	}
        DialogWidget $w_.rtd_status \
		-messagewidth 6i \
		-justify left \
		-text "$t1\n\n$s1\n$s2\n$s3" \
		-title "Status"
        $w_.rtd_status activate
    }
    
    # display a popup window with information about Rtd
    
    public method rtd_about {} {
	global tcl_pkgPath rtd_library tclutil_library astrotcl_library

        set rtdLoaded {}
        foreach d [info loaded] {
            if {[lsearch $d Rtd] != -1} {
                set rtdLoaded [lindex $d 0]
                break
            }
        }

	set t1 "Rtd version:\t[package versions Rtd]"
	set t2 "Rtd shared library:\t[abs_path $rtdLoaded]"
	set t3 "Rtd library path:\t[abs_path $rtd_library]"
	set t4 "Tclutil lib path:\t[abs_path $tclutil_library]"
	set t5 "Astrotcl lib path:\t[abs_path $astrotcl_library]"
	set t6 "Tcl version:\t[info patchlevel]"
	set t7 "Tcl package path:\t$tcl_pkgPath"
	set t8 "Package versions:\t"
	foreach el "Tclx Itcl Itk Tkx BLT" {
	    set t8 "$t8$el[package versions $el] "
	}
        DialogWidget $w_.rtd_about \
		-messagewidth 12i \
		-justify left \
		-text "$t1\n$t2\n$t3\n$t4\n$t5\n$t6\n$t7\n$t8" \
		-title "About"
        $w_.rtd_about activate
    }

    # return absolute pathname

    protected method abs_path { path } {
        set dir [file dirname $path]
        set tail [file tail $path]
        set cwd [pwd]
        set err [catch {set adir [cd $dir ; pwd]}]
        cd $cwd
        if { $err } { return $path }
        return [file join $adir $tail]
    }
    
    # -- public variables (also program options) -- 

    # image file to display
    itk_option define -file file File {}
    
    #  Float the control panel (better real estate control on small displays).
    itk_option define -float_panel float_panel Float_panel 0

    # flag: if true, try to use X shared memory for images
    itk_option define -usexshm useXshm UseXshm {1}

    #flag: if true, try to use X synchronisation
    itk_option define -usexsync useXsync UseXsync {1}

    # This flag controls whether the FITS image header is kept in 
    # sysV shared memory (see the rtdRemote interface for use of this)
    itk_option define -shm_header shm_header Shm_header 0

    # This flag controls whether the FITS image data is kept in 
    # sysV shared memory (see the rtdRemote interface for use of this)
    itk_option define -shm_data shm_data Shm_data 0

    # specify the min number of colors to allocate before using
    # a private colormap (not impl.)
    itk_option define -min_colors min_colors Min_colors 30

    # specify the max number of colors to allocate before using
    # a private colormap (not impl.)
    itk_option define -max_colors max_colors Max_colors 60

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0}

    # flag: if true, use faster subsampling algorithm when shrinking images,
    # otherwise use a pixel algorithm defined by option -sampmethod
    itk_option define -subsample subsample Subsample {1} {
	if {[info exists itk_component(image)]} {
	    $itk_component(image) config -subsample $itk_option(-subsample)
	}
    }

    # sampling method to used when option -subsample is 0
    itk_option define -sampmethod sampmethod Sampmethod {0} {
	if {[info exists itk_component(image)]} {
	    $itk_component(image) config -sampmethod $itk_option(-sampmethod)
	}
    }

    # default (midas) colormap
    itk_option define -default_cmap default_cmap Default_cmap {real}

    # default (midas) intensity transfer table
    itk_option define -default_itt default_itt Default_itt {ramp}

    # camera name: default: $env(RTD_CAMERA), if set, otherwise RTDSIMULATOR
    itk_option define -camera camera Camera {} {
	if {[info exists itk_component(image)]} {
	    $itk_component(image) updateCameraStatus $itk_option(-camera)
	}
    }

    # Panel layout order: set to one of {saoimage reverse default}
    # to change the layout ordering of the panel windows.
    # "saoimage" puts the info first, followed by pan and zoom,
    # "reverse" reverses the default order, which is {zoom info pan}.
    itk_option define -panel_layout panel_layout Panel_layout {}

    # Panel orient: one of {horizontal vertical} (default: horizontal)
    itk_option define -panel_orient panel_orient Panel_orient {}

    # zooming factor
    itk_option define -zoom_factor zoom_factor Zoom_factor 4

    # Width of zoom window
    itk_option define -zoom_width zoom_width Zoom_width 152

    # Height of zoom window
    itk_option define -zoom_height zoom_height Zoom_height 152

    # height of the colorramp subwindow
    itk_option define -colorramp_height colorramp_height Colorramp_height 12

    # width of panning window
    itk_option define -pan_width pan_width Pan_width 152

    # height of panning window
    itk_option define -pan_height pan_height Pan_height 152

    # flag: if true (default), show the color ramp window
    itk_option define -with_colorramp with_colorramp With_colorramp 1

    # flag: if true, use a "view" of the main image for the zoom window
    # otherwise zoom directly from the X display.
    # The advantage of the first approach (-use_zoom_view 1) is that the zoom 
    # is accurate even when the main image is shrunken. 
    # The second (-use_zoom_view 0) is faster and allows more accurate positioning.
    itk_option define -use_zoom_view use_zoom_view Use_zoom_view 1

    # flag: if true, changes in main image scale will propagate to the zoom window,
    # otherwise controls are displayed so the user can manually change it (ZoomView only)
    itk_option define -zoom_view_propagate zoom_view_propagate Zoom_view_propagate 1

    # flag: if true (default) make a zoom window 
    itk_option define -with_zoom_window with_zoom_window With_zoom_window 1

    # flag: if true (default) make a panning window
    itk_option define -with_pan_window with_pan_window With_pan_window 1

    # flag: if true, turn on zoom window
    itk_option define -dozoom dozoom Dozoom 1
    
    # flag: if true, display a copy (view) of the image as an icon
    itk_option define -disp_image_icon disp_image_icon Disp_image_icon 0

    # flag: if true, set bindings to scroll with the middle mouse button
    itk_option define -drag_scroll drag_scroll Drag_scroll 1

    # flag: if true, display scrollbars to scroll the image
    itk_option define -scrollbars scrollbars Scrollbars 0

    # default port for remote connections (0 means system chooses a port)
    itk_option define -port port Port 0
    
    # debugging flag: enables real-time simulation with $testProg (below)
    itk_option define -debug debug Debug 0 {
        global ::env
        if {$itk_option(-debug) == 1} {
	    set cam rtdtest
	    if {[cget -number] > 1} {
		set cam "$cam[cget -number]"
	    }
            set env(RTD_CAMERA) $cam
            configure -camera $cam
        }
    }

    # for testing: name of test program used to generate real-time updates
    itk_option define -testprog testProg TestProg "tRtd"

    # for testing: interval between updates in ms
    itk_option define -interval interval Interval 500

    # with performance tester utility in menu bar
    itk_option define -with_perftest with_perftest With_perftest 1

    # option to warp the mouse pointer
    itk_option define -with_warp with_warp With_warp 1

    # option to include grid button (default to off, since it doesn't work
    # well yet on some images)
    itk_option define -with_grid with_grid With_grid 0

    # minimum allowed scale value
    itk_option define -min_scale min_scale Min_scale -10

    # maximum allowed scale value
    itk_option define -max_scale max_scale Max_scale 20
    
    # Set the default color scale algorithm to one of: {linear log sqrt histeq}
    itk_option define -color_scale color_scale Color_scale linear

    # dummy option, used when cloning the main window, in place of "-file"
    itk_option define -noop noOp NoOp {}

    # -orient option for Pick Object window
    itk_option define -pickobjectorient pickObjectOrient PickObjectOrient {vertical}

    # option to update RtdImagePick after a real-time image event
    itk_option define -updatePick updatePick UpdatePick {1}

    # default scaling factor
    itk_option define -xscale xscale Xscale 1
    itk_option define -yscale yscale Yscale 1

    # attach to camera
    itk_option define -attach attach Attach {0} {
	set attach_ [cget -attach]
    }

    # Rtd's window geometry
    itk_option define -rtd_geometry rtd_geometry Rtd_geometry {} {
	if {"$itk_option(-rtd_geometry)" != ""} {
	    after idle [code "wm geometry $w_ $itk_option(-rtd_geometry)"]
	}
    }

    # Rtd's window title
    itk_option define -rtd_title rtd_title Rtd_title {} {
	if {"$itk_option(-rtd_title)" != ""} {
	    after idle [code "wm title $w_ {$itk_option(-rtd_title)}"]
	}
    }

    # Default directory for loading images
    itk_option define -image_directory image_directory Image_directory {}

    # -- protected variables -- 

    # name of main image (class RtdImageCtrl or a derived class)
    protected variable image_

    # pid of test prog used to generate rapid frames (debug)
    protected variable rapid_pid_

    # attach to camera after widget was initialized
    protected variable attach_ 0
}

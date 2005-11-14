# E.S.O. - VLT project/ESO Archive
# "@(#) $Id: SkyCat.tcl,v 1.2 2005/01/20 23:04:30 brighton Exp $"
#
# SkyCat.tcl - image display application class with catalog extensions
#
# This class defines a top level window for the skycat application.
#
# The easiest way to use this class is via the "startSkyCat" method
#
# See man page SkyCat(1) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 11 Oct 95   created
# P.W.Draper 19 Jan 00   added concat to bindtags, itk ones were
#                        being lost. Removed extra ] from ]] in title string.
#            18 Nov 03   Now accepts a list of catalogues to display.

set skycat_usage {
Usage: skycat ?fitsFile? ?-option value ...?

Options:
 -cat <bool>              - Include ESO/Archive catalog extensions (default).
 -catalog \"<cat1> <cat2>\" - Open windows for the given catalogs on startup.
 -colorramp_height <n>    - height of colorramp window (default: 12).
 -float_panel <bool>      - put info panel in a popup window (default: 0).
 -panel_layout <layout>   - panel layout, one of: "saoimage", "reverse", "default" .
 -panel_orient <orient>   - panel orientation, one of: "horizontal", "vertical"
 -pickobjectorient <v>    - orientation for pick object win: "horizontal", "vertical"    
 -min_scale <n>           - minimum scale for magnification menu (default: -10).
 -max_scale <n>           - maximum scale for magnification menu (default: 20).
 -remote <bool>           - Use existing skycat process, if available, with Tk send.
 -debug <bool>            - debug flag: run bg processes in fg.
 -default_cmap <cmap>     - default colormap.
 -default_itt <itt>       - default intensity transfer table.
 -file <file>             - fits file to load ('-' for stdin).
 -port <port>             - Listen for remote cmds on port (default: 0 = choose port).
 -rtd <bool>              - Include ESO/VLT Real-Time Features.
 -scrollbars <bool>       - Display scrollbars (not displayed by default).
 -shm_data <bool>         - Put image data in sysV shared memory.
 -shm_header <bool>       - Put image header in sysV shared memory.
 -usexshm <bool>          - Use X shared mem, if available (default).
 -use_zoom_view <bool>    - Use a "view" of the image for the zoom window (default).
 -verbose <bool>          - Print diagnostic messages.
 -with_colorramp <bool>   - Display the color bar (default).
 -with_warp <bool>        - add bindings to move mouse ptr with arrow keys (default: 1).
 -with_grid <bool>        - Include a WCS grid button (default: 0 = off).
 -with_pan_window <bool>  - Display the pan window (default).
 -with_zoom_window <bool> - Display the zoom window (default).
 -zoom_factor <n>         - zooming factor (default: 4).
}

set about_skycat "\
Skycat version $skycat_version
Copyright (C) 1996-2001 ESO - European Southern Observatory

Authors: 

Allan Brighton (abrighton@gemini.edu)
Thomas Herlin (therlin@eso.org)
Miguel Albrecht (malbrech@eso.org)
Daniel Durand (durand@dao.nrc.ca)
Peter Biereichel (pbiereic@eso.org)

Please send any comments, suggestions or bug reports to:

Allan Brighton (abrighton@gemini.edu)
"

itk::usual SkyCat {}

# This class defines a top level window for the skycat application.  The
# easiest way to create an instance this class is via the "startSkyCat"
# proc. It sets up the environment, creates an instance of the class and
# then waits for the application to exit.
#
# The SkyCat widget supports the same options as the Rtd widget, its
# base class, and adds some of its own options.  The widget options are
# the same as the skycat command line options, since these are passed
# unchanged to the widget.

itcl::class skycat::SkyCat {
    inherit rtd::Rtd 

    # constructor: create a toplevel window

    constructor {args} {
	eval itk_initialize $args

	if { "$itk_option(-panel_orient)" == "" } {
	    config -panel_orient "vertical"
	}
    }

    
    # called after the options have been evaluated
    
    protected method init {} {
	Rtd::init

	load_toplevel_geometry
	wm title $w_ "Skycat - version [skycat_version] ($itk_option(-number))"
	wm iconname $w_ 
	feedback "catalog and help menu..."
	
	add_go_menu
	add_graphics_save_menu_item

	if {$itk_option(-cat)} {
	    cat::AstroCat::add_catalog_menu \
		$w_ [code $image_] ::skycat::SkySearch $itk_option(-debug)
	}

	if {"$itk_option(-dhshost)" != ""} {
	    add_olaf_menu
	}

	add_help_menu

	if {[winfo exists $w_.init]} {
	    # destroy init window
	    destroy $w_.init
	    
	    # the logo uses up colors: the update forces the destroy and frees the colors
	    update 
	    $itk_component(image) alloccolors 60
	    wm deiconify $w_
	}
	
        if {"$itk_option(-catalog)" != ""} {
            # make sure we use full path name for local catalogs
            # and process option as a list
            foreach f "$itk_option(-catalog)" {
               if {[file exists $f] && "[string index $f 0]" != "/"} {
                   set $f [pwd]/$f
               }
               cat::AstroCat::open_catalog_window $f \
                   [code $image_] ::skycat::SkySearch $itk_option(-debug) $w_
            }
        }
	
	# check the main window size to make sure it is not too large
	bind SkyCat::resize <Configure> [code $this resize %w %h]
	bindtags $w_ [concat SkyCat::resize [bindtags $w_]]
	#bindtags $w_ SkyCat::resize
    }


    # destructor - delete C++ based objects

    destructor {
	save_toplevel_geometry
	catch {$w_.cat delete}
	
	# kill any catalog windows referring to this image window
	foreach w [cat::AstroCat::instances all] {
	    catch {
		if {"[$w cget -id]" == "[code $image_]"} {
		    destroy $w
		}
	    }
	}

	# remove this window from the list of skycat windows
	global ::skycat_images
	if {[info exists skycat_images]} {
	    set tmp {}
	    foreach w $skycat_images {
		if {[winfo exists $w] && "$w" != "$w_"} {
		    lappend tmp $w
		}
	    }
	    set skycat_images $tmp
	}

    }

    
    # save the position of the top level window so we can reload it the next
    # time.

    protected method save_toplevel_geometry {} {
	set s [winfo geometry $w_]
	# check for case where window was not initialized...
	if {"$s" != "1x1+0+0"} {
	    if {[catch {set fd [::open $toplevel_geometry_ w]}]} {
		return
	    }
	    puts $fd $s
	    ::close $fd
	}
    }


    # restore the position of the top level window from the previous session

    protected method load_toplevel_geometry {} {
	if {[catch {set fd [::open $toplevel_geometry_]}]} {
	    return
	}
	catch {wm geometry $w_ [gets $fd]}
	::close $fd
    }


    # Called when the main window is resized:
    # Check the geometry to make sure it fits on the screen.

    protected method resize {w h} {
	bind SkyCat::resize <Configure> { }
	set sw [winfo screenwidth $w_]
	set sh [winfo screenheight $w_]
	if {$w > $sw || $h > $sh} {
	    wm geometry $w_ "[min $w $sw]x[min $h $sh]+0+0"
	}
    }


    # Add the Real-time menubutton and menu, if the -rtd option was given.

    protected method add_realtime_menu {} {
	# add/remove some menus
	if {$itk_option(-rtd)} {
	    Rtd::add_realtime_menu
	} else {
	    # hide the realtime status
	    [[$itk_component(image) component info] component cameraStatus] config \
		-width 0 -height 0
	}
    }


    # Add a "Go" menu with shortcuts to view images previously viewed

    protected method add_go_menu {} {
	set m [add_menubutton "Go" "Go: menu with shortcuts to view images previously viewed"]
	$m config -postcommand [code $image_ update_history_menu $this $m]
    }


    # Add a menu item to the Graphics menu for saving the line graphics in a FITS
    # table in the image.

    protected method add_graphics_save_menu_item {} {
	if {[catch {[$image_ get_image] hdu count} msg]} {
	    # might be a plugin, such as GAIA that doesn't have the HDU features...
	    return
	}
	
	set m [get_menu Graphics]
	$m add separator
	add_menuitem $m command "Save graphics with image" \
	    {Save line graphics in a FITS binary table in the image} \
	    -command [code $image_ save_graphics_with_image]
    }



    # set default X resources for colors and fonts, and set some default key
    # bindings. This method is called from the parent class and overridden here.
    # These are built-in defaults that the user can also override in the ~/.Xdefaults
    # file.

    protected method setXdefaults {} {
	# read rtd defaults
	Rtd::setXdefaults

	# read cat lib defaults
	cat::setXdefaults
	
	# read skycat defaults
	skycat::setXdefaults
	
	# since we know this method gets called early, this is a good place to 
	# create window to display while starting up
	make_init_window
    }


    # add a menubutton with OLAF items

    protected method add_olaf_menu {} {
	set m [add_menubutton "OLAF" "OLAF menu: On-Line Archive Facility functions"]

	add_menuitem $m checkbutton "Subscribe to DHS on $itk_option(-dhshost)" \
	    {Subscribe/Unsubscribe to DHS images} \
	    -variable $w_.subscribe -onvalue 1 -offvalue 0 \
	    -command [code $image_ subscribe $w_.subscribe \
			  $itk_option(-dhshost) $itk_option(-dhsdata)]
    }


    # add a menubutton with help items

    protected method add_help_menu {} {
	set m [add_menubutton "Help" {} right]

	add_menuitem $m command "About Skycat..." \
	    {Display a window with information about this Skycat version} \
	    -command [code $itk_component(image) about]

	add_menuitem $m command "Help..." \
	    {Display information about Skycat in netscape (if netscape is available)} \
	    -command [code $itk_component(image) send_to_netscape $itk_option(-help_url)]

	add_short_help $itk_component(menubar).help \
	    {Help menu: display information about this application}
    }

    
    # make a new main window (redefined from parent class)

    public method clone {} {
	global ::skycat_usage
	# use the -noop option to avoid reloading the main image (part of $argv list)
	after 0 [code util::TopLevelWidget::start skycat::SkyCat "-noop" "$skycat_usage"]
    }


    # create the rtd image widget with catalog extensions
    # (redefined from parent class to use class with catalog
    # features added)

    protected method make_rtdimage {} {
	set image_ $w_.image
	# SkyCatCtrl(n) widget (derived from RtdImageCtrl), for displaying
	# image and control panel
	itk_component add image {
	    SkyCatCtrl $image_ \
		-file $itk_option(-file) \
		-usexshm $itk_option(-usexshm) \
		-shm_header $itk_option(-shm_header) \
		-shm_data $itk_option(-shm_data) \
		-drag_scroll $itk_option(-drag_scroll) \
		-scrollbars $itk_option(-scrollbars) \
		-verbose $itk_option(-verbose) \
		-subsample $itk_option(-subsample) \
		-use_zoom_view $itk_option(-use_zoom_view) \
		-with_zoom_window $itk_option(-with_zoom_window) \
		-with_pan_window $itk_option(-with_pan_window) \
		-zoom_factor $itk_option(-zoom_factor) \
		-colorramp_height $itk_option(-colorramp_height) \
		-color_scale $itk_option(-color_scale) \
		-default_cmap $itk_option(-default_cmap) \
		-default_itt $itk_option(-default_itt) \
		-with_colorramp $itk_option(-with_colorramp) \
		-rapid_frame_command [code $this rapid_frame_command] \
		-feedback [code $this feedback] \
		-port $itk_option(-port) \
		-shorthelpwin $this \
		-debug $itk_option(-debug) \
		-with_grid $itk_option(-with_grid) \
		-with_warp 1 \
		-regioncommand [code $this select_region] \
		-float_panel $itk_option(-float_panel) \
		-panel_layout $itk_option(-panel_layout) \
		-panel_orient $itk_option(-panel_orient) \
		-min_scale $itk_option(-min_scale) \
		-max_scale $itk_option(-max_scale) \
		-pickobjectorient $itk_option(-pickobjectorient)
	}

	# keep a list of skycat instances
	global ::skycat_images
	lappend skycat_images $itk_component(image)
    }

    
    # This method is called when a region of the image has been selected
    # (via -regioncommand option when creating image above).
    # The arguments are the bounding box of the region in canvas coords.
    # pass it on to any catalog windows to select any catalog symbols
    # in the region.

    protected method select_region {x0 y0 x1 y1} {
	foreach w [cat::AstroCat::instances] {
	    $w select_region $x0 $y0 $x1 $y1
	}
    }


    # display a window while the application is starting up
    
    protected method make_init_window {} {
	global ::about_skycat ::skycat_library
	
	set skycat_logo [image create pixmap -id skycat_logo]

	set w [util::TopLevelWidget $w_.init -center 1]
	#catch {rtd_set_cmap $w}
	wm title $w " "
	wm withdraw $w_
	pack \
	    [label $w.logo -image $skycat_logo \
		 -borderwidth 2 -relief groove] \
	    [message $w.msg -text $about_skycat \
		 -width 6i \
		 -justify center \
		 -borderwidth 2 -relief groove] \
	    [ProgressBar $w.progress \
		 -from 0 -to 10 -value 0 \
		 -borderwidth 2 -relief groove] \
	    -side top -fill x -padx 1m -pady 2m
	tkwait visibility $w
    }


    # this method is redefined here to get feedback during startup

    public method feedback {msg} {
	if {[winfo exists $w_.init.progress]} {
	    $w_.init.progress config -text $msg -value [incr percent_done_]
	    update idletasks
	}
    }


    # This method is called for the -remote option. If another skycat is running,
    # use it to display the image and exit, otherwise do it in this process.
    # Try Tk send, and if that fails, fall back on the RTD socket interface.

    protected method start_remote {} {
	global ::argc ::argv ::env

	set name [winfo name .]
	foreach interp [winfo interps] {
	    if {"$interp" != "$name" && [string match "Skycat*" $interp]} {
		
		# command to eval in the remote skycat application
		set cmd [list skycat::SkyCat::remote_start $argc $argv]
		
		# try Tk send
		if {[catch {send $interp $cmd}]} {
		    # failed: try rtd remote socket interface 
		    # (rtd creates the file below on startup with pid, host and port info)
		    set file $env(HOME)/.rtd-remote
		    if {[catch {set fd [open $env(HOME)/.rtd-remote]}]} {
			return
		    }
		    set s [gets $fd]
		    close $fd
		    set status 0
		    if {[scan $s {%d %s %d} pid host port] != 3} {
			return
		    }
		    if {[catch {
			# see if the process is still running
			exec kill -0 $pid
			set fd [server_connect -nobuf $host $port]
		    }]} {
			return
		    }
		    if {[catch {
			# use the rtdimage "remotetcl" subcommand 
			# (see rtd/rtdimg/src/RtdImage.C)
			puts $fd [list remotetcl $cmd]
			lassign [gets $fd] status length
			set result {}
			if {$length > 0} {
			    set result [read $fd $length]
			}
		    }]} {
			close $fd
			return
		    }
		    if {$status != 0} {
			return
		    }
		}
		# looks like we were successful, so we can exit
		exit
	    }
	}
    }
    

    # start the application with the above class as the main window
    # This proc is called from tkAppInit.c when we are running the single
    # binary version.
    # Note that the binary version doesn't need to set auto_path or look for 
    # Tcl sources or colormaps at run-time, since they are already loaded in 
    # the binary.

    public proc startSkyCat {} {
	global ::rtd_library ::skycat_library ::skycat_usage ::tk_strictMotif \
	    ::argv ::argc ::env

	if {! [info exists rtd_library]} {
	    set rtd_library .
	}

	# where to look for catalog config file: 
	# use ~/.skycat/skycat.cfg if it exists, since it may contain user's 
	# preferences, otherwise use $SKYCAT_CONFIG if set, or $CATLIB_CONFIG.
	set config_file $env(HOME)/.skycat/skycat.cfg
	if {[file exists $config_file]} {
	    set env(CATLIB_CONFIG) "file:$config_file"
	} elseif {[info exists env(SKYCAT_CONFIG)]} {
	    set env(CATLIB_CONFIG) $env(SKYCAT_CONFIG)
	}

	tk appname Skycat
	set tk_strictMotif 0
	tk_focusFollowsMouse

	# insert some default options
	set argv [linsert $argv 0 -disp_image_icon 1]
	set argc [llength $argv]

	# specify a list of valid options (workaround for tcl or itcl bug (?) that
	# crashes app if option is unknown...)
	set optlist [list \
		-cat \
		-catalog \
		-color_scale \
		-colorramp_height \
		-debug \
		-default_cmap \
		-default_itt \
		-dhsdata \
		-dhshost \
		-disp_image_icon \
		-drag_scroll \
		-feedback \
		-file  \
		-float_panel \
		-help_url \
		-max_scale \
		-min_scale \
		-panel_layout \
		-panel_orient \
		-pickobjectorient \
		-port \
		-rapid_frame_command \
		-regioncommand \
		-remote \
		-rtd \
		-scrollbars \
		-shm_data \
		-shm_header \
		-shorthelpwin \
		-subsample \
		-use_zoom_view \
		-usexshm \
		-verbose \
		-with_colorramp \
		-with_grid \
		-with_pan_window \
		-with_warp 1 \
		-with_zoom_window \
		-zoom_factor \
		]

	# start the application
	util::TopLevelWidget::start skycat::SkyCat "-file" "$skycat_usage" "" 1 $optlist
    }


    # -- external interface via Tk send --

    # This proc returns the instance name of the catalog (or image server)
    # widget
    # If more than one is open, it asks the user to select which one.
    # If it can't find one, it reports an error and returns ""

    public proc get_catalog {{what "catalog"}} {
	# get list of catalog windows
	if {"$what" == "catalog"} {
	    set list [cat::AstroCat::instances]
	} else {
	    set list [cat::AstroCat::instances imagesvr]
	}

	if {[llength $list] == 0} {
	    error_dialog "There are no $what windows open"
	    return
	}

	if {[llength $list] == 1} {
	    return [lindex $list 0]
	} 

	# need to choose which catalog
	set names {}
	set n 0
	foreach w $list {
	    lappend names "[incr n]  [$w cget -catalog]"
	}
	set w [ChoiceDialog .d \
		   -text "Please specify which $what to use:" \
		   -cols 1 \
		   -messagewidth 3i \
		   -choice $names \
		   -value [lindex $names 0] \
		  ]
	set result [$w activate]
	if {"$result" == ""} {
	    return
	}
	lassign $result n name
	incr n -1
	return [lindex $list $n]
    }


    # This proc returns the instance name of the image server
    # widget
    # If more than one is open, it asks the user to select which one.
    # If it can't find one, it reports an error and returns ""

    public proc get_imagesvr {} {
	return [skycat::SkyCat::get_catalog "image server"]
    }


    # This proc can be called via send from another application to return the contents
    # of the catalog window as a Tcl list. 
    #
    # The format of the return value is {{selected_row} {{row1} {row2} ...}}
    # where each row is a list of column values. The selected_row is empty if there is
    # no selection, otherwise it is a list of column values in the selected row.

    public proc get_catalog_info {} {
	if {"[set w [skycat::SkyCat::get_catalog]]" == ""} {
	    return
	}
	set table [$w component results]
	return [list [lindex [$table get_selected] 0] [$table get_contents]]
    }


    # This proc can be called via send from another application to display an image
    # given the coordinates and a width and height in arcmin

    public proc display_image {ra dec width height {equinox 2000} {catalog "Digitized Sky at ESO"}} {
	global ::skycat_images
	set w [lindex $skycat_images 0]
	cat::AstroCat::open_catalog_window $catalog \
	    $w ::skycat::SkySearch 0 [winfo toplevel $w]
	update
	if {"[set w [get_imagesvr]]" == ""} {
	    return
	}
	$w getimage_from_args $ra $dec {} $equinox $width $height
    }


    # This proc can be called via send from another application to display a catalog
    # given the catalog's name (long name or short name).

    public proc display_catalog {{catalog "Guide Star Catalog at ESO"}} {
	global ::skycat_images
	set w [lindex $skycat_images 0]
	cat::AstroCat::open_catalog_window $catalog \
	    $w ::skycat::SkySearch 0 [winfo toplevel $w]
    }



    # This proc can be called via send from another application to display a rectangle
    # on the image at the given center coords with the given width and height
    # and return the item's canvas tag or id

    public proc mark_image {ra dec width height} {
	global ::skycat_images
	#return [[winfo command [lindex $skycat_images 0]] mark_image $ra $dec $width $height]
	return [[lindex $skycat_images 0] mark_image $ra $dec $width $height]
    }


    # remove the given mark from the image (id returned from mark_image)

    public proc unmark_image {id} {
	global ::skycat_images
	# [winfo command [lindex $skycat_images 0]] unmark_image $id
	[lindex $skycat_images 0] unmark_image $id
    }


    # This proc can be called via send to load a fits image for viewing

    public proc load_image {filename} {
	global ::skycat_images
	#if {[catch {[winfo command [lindex $skycat_images 0]] config -file $filename} msg]} {
	#    error_dialog $msg
	#}
	if {[catch {[lindex $skycat_images 0] config -file $filename} msg]} {
	    error_dialog $msg
	}
    }


    # pop up a window, ask the user to select an object in the image,
    # wait for the selection and return the info for it in the form:
    # {$x $y $ra $dec $equinox $fwhmX $fwhmY $angle $object $background}
    # 
    # An optional Tcl command may be specifed to be called whenever a new
    # object is selected. The command can include a "send ..." prefix to
    # call a proc in another application

    public proc pick_object {{cmd ""}} {
	global ::skycat_images
	set var [lindex $skycat_images 0].pick.picked
	global ::$var
	catch {unset $var}
	#if {[catch {[winfo command [lindex $skycat_images 0]] pick_dialog $cmd} msg]} {
	#    error_dialog $msg
	#    return
	#}
	if {[catch {[lindex $skycat_images 0] pick_dialog $cmd} msg]} {
	    error_dialog $msg
	    return
	}
	if {! [info exists $var]} {
	    tkwait variable $var
	}
	return [set $var]
    }


    # return a list of SkyCatCtrl class instances in this process (there might be
    # multiple cloned instances...)

    public proc get_skycat_images {} {
	global ::skycat_images
	if {[info exists skycat_images]} {
	    return $skycat_images
	}
    }


    # This proc is called via Tcl send from a remote skycat application when the
    # -remote option is used. The arguments are the argc and argv of the remote
    # skycat application. We extract the file and catalog arguments and ignore 
    # the rest, since we are reusing the same window.

    public proc remote_start {ac av} {
	# get the image file option from the argv list
	set file {}
	set catalog {}
	for {set i 0} {$i < $ac} {incr i} {
	    set opt [lindex $av $i]
	    if {"[string index $opt 0]" == "-" && "$opt" != "-"} {
		set arg [lindex $av [incr i]]
	    } else {
		set arg $opt
		set opt "-file"
	    }
	    if {"$opt" == "-file"} {
		set file $arg
	    } elseif {"$opt" == "-catalog"} {
		set catalog $arg
	    }
	}

	# open a new main window using the new arguments
	foreach w [get_skycat_images] {
	    if {[winfo exists $w]} {
		if {"$file" != ""} {
		    if {[file exists $file]} {
			$w configure -file $file
		    } else {
			error_dialog "File does not exist: $file"
			return
		    }
		}
		if {"$catalog" != ""} {
		    # open a window for the given catalog
		    cat::AstroCat::open_catalog_window $catalog \
			$w ::skycat::SkySearch 0 [winfo toplevel $w]
		}
		return
	    }
	}
    }
    

    # -- options --
    
    # flag: if true, display the data-servers menu (catalog features)
    itk_option define -cat cat Cat 1 {
	if {$itk_option(-cat) != 0 && $itk_option(-cat) != 1} {
	    set itk_option(-cat) 1
	    puts "The -cat option requires a value of 1 (true) or 0 (false)"
	    exit 1
	}
    }

    # flag: if true, display the real-time menu (VLT features)
    itk_option define -rtd rtd Rtd 0 {
	if {$itk_option(-rtd) != 0 && $itk_option(-rtd) != 1} {
	    set itk_option(-rtd) 1
	    puts "The -rtd option requires a value of 1 (true) or 0 (false)"
	    exit 1
	}
    }

    # Specify a catalog (may be a local file) to load on startup
    itk_option define -catalog catalog Catalog {}

    # For OLAF (On-Line Archive Facility): name of DHS host machine
    itk_option define -dhshost dhshost DhsHost {}

    # directory used to hold image files from OLAF/DHS
    itk_option define -dhsdata dhsdata DhsData {}

    # url to use for the help menu - link to skycat WWW page
    itk_option define -help_url help_url Help_url {http://archive.eso.org/skycat}

    # if another skycat application is running on this display, use
    # it rather than this process (saves memory and colors in the colormap).
    itk_option define -remote remote Remote 0 {
	if {"$itk_option(-remote)" == "1"} {
	    start_remote
	}
    }

    # -- protected variables --
    
    # used in startup dialog 
    protected variable percent_done_ 0

    # name of the file used to save the positions of the top level windows
    global ::env
    protected common toplevel_geometry_ $env(HOME)/.skycat/geometry
}


# The following procs are now member procs of the SkyCat class, but are
# defined here as wrappers for backward compatibility. See the member procs
# above for the definitions.

proc startSkyCat {} {
    skycat::SkyCat::startSkyCat
}

proc get_catalog {{what "catalog"}} {
    return [skycat::SkyCat::get_catalog $what]
}

proc get_imagesvr {} {
    return [skycat::SkyCat::get_imagesvr]
}

proc get_catalog_info {} {
    return [skycat::SkyCat::get_catalog_info]
}

proc display_image {ra dec width height {equinox 2000} {catalog "Digitized Sky at ESO"}} {
    return [skycat::SkyCat::display_image $ra $dec $width $height $equinox $catalog]
}

proc display_catalog {{catalog "Guide Star Catalog at ESO"}} {
    return [skycat::SkyCat::display_catalog $catalog]
}

proc mark_image {ra dec width height} {
    return [skycat::SkyCat::mark_image $ra $dec $width $height]
}

proc unmark_image {id} {
    return [skycat::SkyCat::unmark_image $id]
}

proc load_image {filename} {
    return [skycat::SkyCat::load_image $filename]
}

proc pick_object {{cmd ""}} {
    return [skycat::SkyCat::pick_object $cmd]
}

proc get_skycat_images {} {
    return [skycat::SkyCat::get_skycat_images]
}


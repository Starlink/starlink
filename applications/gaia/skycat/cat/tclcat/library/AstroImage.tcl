# E.S.O. - VLT project/ESO Archive
# @(#) $Id: AstroImage.tcl,v 1.25 1998/11/20 14:19:35 abrighto Exp $
#
# AstroImage.tcl - user interface class for accessing image servers
#                     such as DSS (Digitized Sky Survey) 
#
# See man page AstroImage(1) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 11 Oct 95   created

itk::usual AstroImage {}

# AstroImage is a user interface class for accessing image servers
# such as DSS (Digitized Sky Survey).

itcl::class cat::AstroImage {
    inherit util::TopLevelWidget

    # constructor

    constructor {args} {
	eval itk_initialize $args
    }


    # called after options have been evaluated

    protected method init {} {
	global ::env
	if {"$itk_option(-image)" == ""} {
	    error_dialog "Can't create AstroImage widget: no skycat image widget was specified"
	    destroy $w_
	    return
	}
	
	wm title $w_ "Image Servers ($itk_option(-number))"
	
	# set the file used to keep track of URLs (for debugging)
	set_logfile

	# do window layout
	layout_dialog

 	# set up feedback for HTTP 
	##set_feedback on

	# add a short help window
	make_short_help

	# create an object for running interruptable batch image requests
	Batch $w_.batch \
	    -command [code $this request_done] \
	    -debug $itk_option(-debug)

	set initialized_ 1
	config -image $itk_option(-image)
    }


    # destructor - delete C++ based objects so that the temp image
    # files are deleted

    destructor {
	catch {$w_.im delete}
	catch {file delete $itk_option(-tmpfile)}
	##set_feedback off
    }


    # add the menu bar

    protected method add_menubar {} {
	TopLevelWidget::add_menubar

	set m [add_menubutton File]

	add_menuitem $m command "Close" \
	    {Close this window} \
	    -command [code $this quit]
	
	set m [add_menubutton Options]
	add_menuitem $m cascade "Name Server" \
	    {Select the name server used to resolve astronomical object names} \
	    -menu [set ns_menu [menu $itk_component(options).m.ns]]

	get_name_servers $ns_menu

	# ImageServers menu
	set m [add_menubutton ImageServers]
	fill_imagesvr_menu $m imagesvr
    }


    # Fill up a menu of known data servers to choose from.
    # The serv_type argument should be one of: "catalog", "archive", "imagesvr".

    public method fill_imagesvr_menu {m {serv_type "imagesvr"}} {
	if {[catch {set imagesvr_list [lsort [$w_.im info $serv_type]]} msg]} {
	    error_dialog $msg $w_
	    return
	}
	
	if {[llength $imagesvr_list]} {
	    foreach i $imagesvr_list {
		add_menuitem $m command $i \
		    "Open $serv_type: \"$i\"" \
		    -command [code $this select_imagesvr $i]
	    }
	}
    }

    # called when an imagesvr ($name) is selected from the menu.

    public method select_imagesvr {name} {
	cat::AstroImage::new_imagesvr $name $itk_option(-image) $itk_option(-debug)
    }


    
    # create the ~/.skycat dir if it does not already exists and keep a log
    # file there.

    protected method set_logfile {} {
	global ::env

	# open log file used to keep track of URLs (for debugging)
	set dir $env(HOME)/.skycat

	if {! [file isdirectory $dir]} {
	    catch {mkdir $dir}
	}
	set logfile_name_ $dir/log
    }

    
    # quit and close the window

    public method quit {} {
	if {$itk_option(-standalone)} {
	    destroy $w_
	} else {
	    wm withdraw $w_
	}
    }



    # add the name server catalogs to the given menu

    public method get_name_servers {m} {
	if {[catch {set list [$w_.im info namesvr]} msg]} {
	    error_dialog $msg $w_
	    return
	}

	foreach namesvr_ $list {
	    $m add radiobutton \
		-label $namesvr_ \
		-command [code $this set_namesvr $namesvr_] \
		-variable $m
	}

	# set default name server
	global ::$m
	set $m $namesvr_
    }
    

    # do the dialog window layout
    
    protected method layout_dialog {} {
	# create astroimage object
	astroimage $w_.im

	add_menubar

	pack [frame $w_.main] \
	    -side top -fill both
	pack [frame $w_.main.top -relief groove -borderwidth 2] \
	    -side top -fill x

	pack [set f [frame $w_.main.top.f -relief groove -borderwidth 2]] \
	    -side top -fill x -pady 1m
	blt::table $f

	blt::table $f \
	    [set name_ [LabelEntry $f.name \
		 -text "Object Name:" \
		 -command [code $this getimage] \
		 -labelwidth $itk_option(-labelwidth) \
		 -valuewidth $itk_option(-valuewidth) \
		 -anchor e \
		 -valuefont $itk_option(-valuefont) \
		 -labelfont $itk_option(-labelfont)]] \
	    0,0 -fill x -pady 1m \
	    [set equinox_ [LabelEntry $f.equinox \
		 -text "Equinox:" \
		 -autoselect 1 \
		 -value "J2000" \
		 -command [code $this getimage] \
		 -labelwidth $itk_option(-labelwidth) \
		 -valuewidth $itk_option(-valuewidth) \
		 -anchor e \
		 -valuefont $itk_option(-valuefont) \
		 -labelfont $itk_option(-labelfont)]] \
	    0,1 -fill x -pady 1m \
	    [set ra_ [LabelEntry $f.ra \
		 -text "a:" \
		 -autoselect 1 \
		 -command [code $this getimage] \
		 -labelwidth $itk_option(-labelwidth) \
		 -valuewidth $itk_option(-valuewidth) \
		 -anchor e \
		 -valuefont $itk_option(-valuefont) \
		 -labelfont $itk_option(-wcsfont)]] \
	    1,0 -fill x -pady 1m \
	    [set dec_ [LabelEntry $f.dec \
		 -text "d:" \
		 -autoselect 1 \
		 -command [code $this getimage] \
		 -labelwidth $itk_option(-labelwidth) \
		 -valuewidth $itk_option(-valuewidth) \
		 -anchor e \
		 -valuefont $itk_option(-valuefont) \
		 -labelfont $itk_option(-wcsfont)]] \
	    1,1 -fill x -pady 1m \
	    [set width_ [LabelEntry $f.width \
		 -text "Width in" \
		 -command [code $this getimage] \
		 -labelwidth [expr $itk_option(-labelwidth)-3] \
		 -valuewidth $itk_option(-valuewidth) \
		 -anchor e \
		 -valuefont $itk_option(-valuefont) \
		 -labelfont $itk_option(-labelfont)]] \
	    2,0 -fill x -pady 1m \
	    [set height_ [LabelEntry $f.height \
		 -text "Height in" \
		 -command [code $this getimage] \
		 -labelwidth [expr $itk_option(-labelwidth)-3] \
		 -valuewidth $itk_option(-valuewidth) \
		 -anchor e \
		 -valuefont $itk_option(-valuefont) \
		 -labelfont $itk_option(-labelfont)]] \
	    2,1 -fill x -pady 1m \
	    [set setf [frame $f.setf]] \
	    3,1 -anchor e -fill x -pady 1m \
	    [set copyright_ [LabelValue $f.copyright \
		 -anchor w -relief flat \
		 -labelwidth 0 -valuewidth 0 \
		 -valuefont $itk_option(-labelfont)]] \
	    4,0 -anchor e -fill x -pady 1m -columnspan 2

	# add RA and DEC labels to width and height (different font)
	pack [label $f.width.ra -text "a:" -font $itk_option(-wcsfont)] \
	    -side left -after $f.width.label -padx 1
	pack [label $f.height.dec -text "d:" -font $itk_option(-wcsfont)] \
	    -side left -after $f.height.label -padx 2

	# add buttons for setting coordinates
	pack \
	    [set setfrom_ [button $setf.setfrom \
			       -text "Set From Image" \
			       -command [code $this set_default_values]]] \
	    [set selectarea_ [button $setf.selectarea \
			       -text "Select Area..." \
			       -command [code $this select_area]]] \
	    -side right -padx 1m -pady 2m


	blt::table configure $f C1 -padx 2m

	# dialog buttons
	pack [frame $w_.buttons -borderwidth 2 -relief groove] \
	    -side top -fill x
	pack \
	    [button $w_.getimage \
		 -text "Get Image" \
		 -command [code $this getimage]] \
	    [button $w_.stop \
		 -text "Stop" \
		 -state disabled \
		 -command [code $this interrupt]] \
	    [button $w_.close \
		 -text "Close" \
		 -command "wm withdraw $w_" ] \
	    -side left -expand 1 -pady 2m -in $w_.buttons

	# set the image server (default if not set)
	if {"$itk_option(-imagesvr)" == ""} {
	    if {[catch {set imagesvr_list [$w_.im info imagesvr]} msg]} {
		error_dialog $msg $w_
		return
	    }
	    set itk_option(-imagesvr) [lindex $imagesvr_list [expr [llength $imagesvr_list]-1]]
	}
	set_imagesvr $itk_option(-imagesvr)

	# add a help button, if there is a help URL
	if {"[$w_.im help]" != ""} {
	    pack \
		[button $w_.help \
		     -text "Help" \
		     -command [code $this help]] \
		-side left -expand 1 -pady 2m -in $w_.buttons -before $w_.close 
	}

	# ProgressBar(n) widget, to display the progress of a query
	itk_component add progress {
	    ProgressBar $w_.progress
	}
	pack $itk_component(progress) -side bottom -fill x
    }

    
    # if the catalog has a "help" URL, try to display it in netscape using the
    # netscape remote interface.

    public method help {} {
	set url [$w_.im help]
	if {"$url" != ""} {
	    cat::AstroCat::send_to_netscape $url
	} else {
	    info_dialog "Sorry, no help is available for this image server"
	}
    }


    # add a short help window and set the help texts
    
    protected method make_short_help {} {
	TopLevelWidget::make_short_help

	add_short_help name_ {SIMBAD object name: resolved via SIMBAD server, if given}
	add_short_help $equinox_ {World Coordinates equinox, default J2000}
	add_short_help $ra_ {World Coordinates: right ascension (RA)}
	add_short_help $dec_ {World Coordinates: declination (DEC)}
	add_short_help $width_ {Width of image in World Coordinates (arcmin)}
	add_short_help $height_ {Height of image in World Coordinates (arcmin)}
	add_short_help $setfrom_ {Set default values from the image}

	add_short_help $w_.getimage {{bitmap b1} = start image request}
	add_short_help $w_.close {{bitmap b1} = close this window}
	add_short_help $w_.stop {{bitmap b1} = interrupt the current image fetch}
    }


    # set the name server to use to resolve object names
    
    public method set_namesvr {name} {
	set namesvr_ $name
    }
    

    # interrupt the current request 

    public method interrupt {} {
	$w_.batch interrupt
	set_feedback off
	catch {file delete $itk_option(-tmpfile)}
	set_state normal
    }

    
    # set/reset widget states while busy 

    public method set_state {state} {
	set state_ $state
	if {"$state" == "normal"} {
	    catch {blt::busy release $w_.main}
	    catch {focus -lastfor $w_.main}
	    $w_.getimage config -state normal
	    $w_.stop config -state disabled
	} else {
	    catch {focus .}
	    catch {blt::busy hold $w_.main}
	    $w_.getimage config -state disabled
	    $w_.stop config -state normal
	}
	update idletasks
	$w_.progress reset
    }

   
     # open or close a pipe to get feedback during HTTP transfers.
    # (To save limited fds, we close the feedback pipe after each HTTP op).
    # The arg should be "on" to turn feedback on, or "off" to turn it off.


    protected method set_feedback {onoff} {
	# Process any pending file events.
	# Note: this is important: if we don't process the events before
	# closing the feedback file, a crash may result.
	update 

	if {$itk_option(-debug)} {
	    # if -debug was given, the feedback bit wont work, since the
	    # query is done in the foreground and trying to read the feedback
	    # from the pipe may cause the application to hang...
	    return
	}
	if {"$onoff" == "on"} {
	    lassign [pipe] rfd_ wfd_
	    fileevent $rfd_ readable [code $this feedback]
	    $w_.im feedback $wfd_
	} elseif {[info exists rfd_]} {
	    ::close $rfd_
	    ::close $wfd_
	    unset rfd_ wfd_
	    $w_.im feedback {}
	}
    }


    # this method is called by the fileevent handler during the image transfer 
    # from the HTTP C++ class to give the user feedback about how much there is 
    # left to copy, etc...
    # Read a line from the feedback pipe, which should contain text to be displayed
    # in the progress widget.

    public method feedback {} {
	set text [gets $rfd_]
	if {"$state_" != "normal"} {
	    if {[scan $text {total length: %d bytes} n] == 1} {
		$w_.progress config -to $n
	    } elseif {[scan $text {read %d bytes} n] == 1} {
		$w_.progress config -value $n
	    } elseif {[scan $text {url: %s} url] == 1} {
	    	catch {
		    set fd [open $logfile_name_ a+]
		    puts $fd "[clock format [clock seconds]]\n$itk_option(-imagesvr)\n$url\n\n"
		    ::close $fd
		}
	    }
	}
	$w_.progress config -text $text
	update idletasks
    }
    

    # This method is called when the background request is done.
    # The arguments are the error status (0 is OK) and the result,
    # which is a list of {filename center-pos copyright}

    protected method request_done {status result} {
	set_feedback off

	if {$status} {
	    # check if we need user/passwd info. errmsg should have the format:
	    # "Authorization Required for <realm> at <host>"
	    if {[regsub {Authorization Required} $result {Enter username} msg]} {
		lassign [passwd_dialog $msg] username passwd
		if {"$username" != "" && "$passwd" != ""} {
		    lassign $result {} {} {} realm {} host
		    $w_.im authorize $username $passwd $realm $host
		    getimage
		}
	    } else {
		set_state normal
		error_dialog $result $w_
		after 0 [list $w_.progress config -text $result]
	    }
	} else {
	    lassign $result filename pos copyright
	    $w_.progress config -text "Loading Image..."
	    $copyright_ configure -value $copyright
	    update idletasks
	    
	    # load the image
	    if {[catch {$itk_option(-image) config -file $filename} msg]} {
		error_dialog $msg
	    }
	    catch {file delete $filename}
	    if {[llength $pos] >= 2} {
		lassign $pos ra dec equinox
		$ra_ config -value $ra
		$dec_ config -value $dec
		$name_ config -value ""
	    }
	    set_state normal
	}
    }


    # set (open) the current image server - called when user selects a 
    # name from the menu
    
    public method set_imagesvr {name} {
	config -imagesvr $name
	wm title $w_ "$name ($itk_option(-number))"
	wm iconname $w_ [$w_.im shortname $name]
	$w_.im open $name
	$copyright_ configure -value [$w_.im copyright]

	set image $itk_option(-image)
	set instance_idx "$name,$image"
	set instances_($instance_idx) $w_ 
    }

   
    # member proc (like a static member function) to create an instance of this
    # class for the named imagesvr (or reuse the existing one for the imagesvr)
    #
    # The args are: 
    #
    #   name  - long name of imagesvr from config file
    #
    #   image - name of skycat itcl image widget
    #
    #   debug - flag: if true, run queries in foreground
    #
    #   w should be the top level window of the caller (optional).

    public proc new_imagesvr {name image debug {w ""}} {
	set i "$name,$image"
	if {[info exists instances_($i)] && [winfo exists $instances_($i)]} {
	    utilRaiseWindow $instances_($i)
	    return
	}

	# If $w was specified, put the window under that top level window
	# so we get the window numbers right (for cloning, see TopLevelWidget).
	if {[winfo exists $w]} {
	    set instname $w.ai[incr n_instances_]
	} else {
	    set instname .ai[incr n_instances_]
	}

	set instances_($i) \
	    [AstroImage $instname \
		 -image $image -debug $debug -imagesvr $name -transient 0]
    }
    
    
    # return a list of the instances of this class

    public proc instances {} {
	set list {}
	if {[info exists instances_]} {
	    foreach i [array names instances_] {
		if {[winfo exists $instances_($i)]} {
		    lappend list $instances_($i)
		}
	    }
	}
	return $list
    }

    
    # Get the requested image from the image server based on the current options
    # and display the resulting image (args are ignored here)

    public method getimage {args} {
	getimage_from_args \
	    [$ra_ get] [$dec_ get] [$name_ get] [$equinox_ get] \
	    [$width_ get] [$height_ get] 0
    }
    
   
    # Get the requested image from the image server based on the given arguments
    # for the world coord position or name and the given width and height
    # if flag is 1, the entries are updated from the arguments.

    public method getimage_from_args {ra dec name equinox width height {flag 1}} {
	if {$flag} {
	    $ra_ config -value $ra
	    $dec_ config -value $dec
	    $name_ config -value $name
	    $equinox_ config -value $equinox
	    $width_ config -value $width
	    $height_ config -value $height
	}

	catch {file delete $itk_option(-tmpfile)}
	# generate a new tmpfile name
	config -tmpfile {}

	set cmd "$w_.im getimage -tmpfile $itk_option(-tmpfile)"
	if {"$equinox" != ""} {
	    lappend cmd "-equinox" $equinox
	}
	if {"$name" != ""} {
	    lappend cmd "-nameserver" $namesvr_ "-name" $name
	} elseif {"$ra" != "" && "$dec" != ""} {
	    lappend cmd "-pos" [list $ra $dec]
	} else {
	    warning_dialog "Please specify either an object name or a position in WCS" $w_
	    return
	}
	lappend cmd -width $width
	lappend cmd -height $height

	# start the request in the background
	set_state disabled
	$w_.progress config -text "Attempting to contact image server..."
	$w_.progress look_busy

	# set up the feedback pipe
	set_feedback on

	$w_.batch bg_eval [code [format {
	    list [%s] [%s.im centerpos] 
	} $cmd $w_]]
    }
    

    # Set the default values for the form entries:
    # set the default position (RA, DEC) to the image center, 

    public method set_default_values {} {
	$name_ config -value ""
	if {[$w_.im iswcs]} {
	    set center  [$image_ wcscenter]
	    if {[llength $center] == 0 || [$image_ isclear]} {
		return
	    }
	    lassign $center ra dec equinox
	    set width [$image_ wcswidth]
	    set height [$image_ wcsheight]
	    set_pos_width_height [list $ra $dec $equinox $width $height]
	} else {
	    set width [$image_ width]
	    set height [$image_ height]
	    set x [expr $width/2.]
	    set y [expr $height/2.]
	    set_pos_width_height [list $x $y $width $height]
	}
    }


    # set the values for the position, width and height entries from the 
    # given list, which should be in the format {ra dec equinox width height} if
    # we are using wcs, or {x y width height} for image pixel coords.

    public method set_pos_width_height {list} {
	set n [llength $list]
	if {[$w_.im iswcs] && $n == 5} {
	    # using world coords 
	    lassign $list ra dec equinox width height
	    $ra_ config -value $ra
	    $dec_ config -value $dec
	    $equinox_ config -value $equinox
	    $width_ config -value [format "%.2f" $width]
	    $height_ config -value [format "%.2f" $height]
	} elseif {[$w_.im ispix] && $n == 4}  {
	    # using image coords
	    lassign $list x y width height
	    $x_ config -value $x
	    $y_ config -value $y
	    $equinox_ config -value ""
	    $width_ config -value [format "%.2f" $width]
	    $height_ config -value [format "%.2f" $height]
	}
    }


    # Ask the user to select an area to search interactively
    # and insert the resulting radius and center pos in the
    # catalog window.

    public method select_area {} {
	if {[$image_ isclear]} {
	    return
	}
	set_pos_width_height [select_image_area [$w_.im iswcs]]
    }


    # convert the given input coordinates in the given input units to the
    # given output units and return a list {x y} with the new values.
    # The units may be one of {canvas image wcs deg "wcs $equinox", "deg $equinox"}

    public method convert_coords {in_x in_y in_units out_units} {
	return [$image_ convert coords $in_x $in_y $in_units {} {} $out_units]
    }


    # convert the given input distance in the given input units to the
    # given output distance and return a list {x y} with the new values.
    # The units may be one of {canvas image wcs deg "wcs $equinox", "deg $equinox"}

    public method convert_dist {in_x in_y in_units out_units} {
	return [$image_ convert dist $in_x $in_y $in_units {} {} $out_units]
    }


    # return the current equinox, or an empty string if the imagesvr does 
    # not support world coordinates. The user can change the equinox used
    # to display coordinates by typing in a different value in the panel.

    public method get_imagesvr_equinox {} {
	if {[$w_.im iswcs]} {
	    set equinox [$equinox_ get]
	    if {"$equinox" == ""} {
		set equinox J2000
	    }
	    return $equinox
	}
    }


    # Ask the user to select an area of the image by dragging out a region
    # on the image return the resulting center pos, width and height as a list of
    # {x y width height}, or {ra dec equinox width height} if wcs_flag 
    # is 1. An empty string is returned if there is no image or the user 
    # cancels the operation.

    public method select_image_area {wcs_flag} {
	if {"$image_" == ""} {
	    return
	}
	
	if {[$image_ isclear]} {
	    error_dialog "No image is currently loaded"
	    return
	}

	# get canvas coords of selected area
	set list [$itk_option(-image) select_area]
	if {[llength $list] != 4} {
	    return
	}
	lassign $list x0 y0 x1 y1
	
	# get center and radius in canvas coords
	set x [expr ($x0+$x1)/2.]
	set y [expr ($y0+$y1)/2.]
	set w [expr abs($x1-$x0)]
	set h [expr abs($y1-$y0)]

	if {$wcs_flag} {
	    # using world coords 
	    set equinox [get_imagesvr_equinox]
	    if {[catch {
		lassign [convert_coords $x $y canvas "wcs $equinox" ] ra dec
		lassign [convert_dist $w $h canvas "deg $equinox" ] width height
	    } msg]} {
		error_dialog "error converting canvas ($x, $y) to world coordinates: $msg" $w_
		return
	    }
	    return [list $ra $dec $equinox [expr $width*60.] [expr $height*60.]]
	} else {
	    # using image coords
	    if {[catch {
		lassign [convert_coords $x $y canvas image] xi yi
		lassign [convert_dist $w $h canvas image] width height
	    } msg]} {
		error_dialog "error converting canvas ($x, $y) to world coordinates: $msg" $w_
		return
	    }
	    return [list $x $y $width $height]
	}
    }

   
    # -- options --

    # default image server name 
    itk_option define -imagesvr imagesvr Imagesvr "dss@eso"

    #  image handle (itcl class: SkyCat or derived)
    itk_option define -image image Image {} {
	if {$initialized_} {
	    set canvas_ [$itk_option(-image) get_canvas]
	    set image_ [$itk_option(-image) get_image]
	    set_default_values
	}
    }

    # font to use for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*

    # font to use for values
    itk_option define -valuefont valueFont ValueFont -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*

    # font to use for ra,dec labels
    itk_option define -wcsfont wcsFont WcsFont -*-symbol-*-*-*-*-14-*-*-*-*-*-*-*

    # set the width for  displaying labels
    itk_option define -labelwidth labelWidth LabelWidth 14

    # set the width for  displaying labels
    itk_option define -valuewidth valueWidth ValueWidth 14

    # set the anchor for labels
    itk_option define -anchor anchor Anchor e

    # text for  label displayed at top
    itk_option define -title title Title "Image Request Parameters"

    # flag: if true, run queries in foreground for better debugging
    itk_option define -debug debug Debug 0

    # temp image file to use
    itk_option define -tmpfile tmpfile Tmpfile {} {
	if {"$itk_option(-tmpfile)" == ""} {
	    set itk_option(-tmpfile) "/tmp/im[pid].$itk_option(-number).[incr count_]"
	}
    }
    
    
    # -- protected members --

    #  canvas window containing main image
    protected variable canvas_

    # internal rtdimage widget for main image
    protected variable image_

    # name field widget
    protected variable name_

    # equinox field widget
    protected variable equinox_

    # ra field widget
    protected variable ra_

    # dec field widget
    protected variable dec_

    # width field widget
    protected variable width_

    # height field widget
    protected variable height_

    # "set from image" field widget
    protected variable setfrom_

    # copyright field
    protected variable copyright_

    # pipe to read feedback during image transfer
    protected variable rfd_

    # pipe to write feedback during image transfer
    protected variable wfd_

    # name server to use to resolve object names
    protected variable namesvr_ {}

    # current state: normal, disabled (i.e.: waiting)
    protected variable state_ {normal}

    # log file handle (used to log URLs)
    protected variable logfile_ stdout

    # flag: set at end of constructor
    protected variable initialized_ 0

    # count used for filename generation
    protected variable count_ 0

    # log file handle (used to log URLs)
    protected variable logfile_name_ 

    # -- common variables (shared by all instances) --
    
    # array mapping catalog name to widget/class name
    protected common instances_

    # instance count
    protected common n_instances_ 0

}



#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdRemoteTcl.tcl,v 1.9 1998/11/20 14:19:47 abrighto Exp $"
#
# RtdRemoteTcl.tcl - itcl widget testing the remote Tcl interface
#                    and some Rtd functions
# 
# See man page RtdImagePopup(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Peter Biereichel 29/07/97  created
# Allan Brighton 16/10/97 moved 2 procs from tclutil.tcl to here
#                         (tclutil.tcl is generic and doesn't know about rtd)
#
# XXX this file should be in the ../test dir... (allan)

itk::usual RtdRemoteTcl {}

# RtdRemoteTcl is an itcl widget for testing the remote Tcl interface and
# some Rtd functions.

itcl::class rtd::RtdRemoteTcl {
    inherit util::TopLevelWidget

    constructor {args} {
        eval itk_initialize $args

	# open the connection to Rtd
	set rtd_fd [connect_to_rtd]
	if {$rtd_fd == 0} {
	    exit 1
	}
	make_buttons
    }

    # This method is called after the constructors have completed.

    protected method init {} {
	global ::rtd_library
	# image widget names
	set image [etcl "$itk_option(-rtd) component image" ]
	# load a FITS file, scale to 1 and set autocut
	etcl "$image config -file $rtd_library/../images/$itk_option(-file)"
	etcl "$image scale 1 1"
    }

    # create actions buttons

    protected method make_buttons {} {
	# action frame
	itk_component add action {
	    frame $w_.action -relief raised -borderwidth 2
	}
	pack $itk_component(action) -expand 1 -fill x
	
	set i 0
	set cmds_ "fliprotate zoom colors cut pixtab fitsh rapid record perft cuts pick"
	set text [list "Flip/Rotate test" "Zoom test" "Color test" "Cut levels test" \
		      "Test Pixel Table" "Fits header test"  "Rapid Frame test" \
		      "Recorder test" "Performance test" "Test Spectrum" "Test Pick Object" ]
	foreach el [set cmds_] {
	    # $el is one of: fliprotate zoom colors cut pixtab fitsh rapid 
	    # record perft cuts pick
	    itk_component add $el {
		button $w_.$el \
		    -text "[lindex $text $i]" \
		    -command [code $this $el] \
		    -anchor w
	    } {
            keep -state
	    }

	    blt::table $itk_component(action) \
		$itk_component($el) $i,0 -anchor w -fill x -columnspan 2
	    incr i
	}
	# "Test all above" button
	itk_component add testall {
	    button $w_.testall \
		-text "Test all above" \
		-command [code $this testall]
	}
	# Cancel button
	itk_component add cancel {
	    button $w_.cancel \
		-text "Cancel" \
		-command [code $this cancel]
	}
	# Close button
	itk_component add close {
	    button $w_.close \
		-text "Close" \
		-command [code destroy $w_]
	}
	blt::table $itk_component(action) \
	    $itk_component(testall) $i,0 -anchor w -fill x -columnspan 2 \
	    $itk_component(cancel) [expr $i+1],0 -anchor w -fill x \
	    $itk_component(close) [expr $i+1],1 -anchor w -fill x
    }

    # flip and rotate

    public method fliprotate {} {
	set trans [etcl "$image component info component trans" ]
	foreach el {flipx flipy rotate} {
	    foreach counter {1 2} {
		if [testcancel] {return}
		etcl "\[$trans component $el\] invoke; update idletasks"
	    }
	    $itk_component(fliprotate) flash
	}
    }

    # spectrum window

    public method cuts {} {
	etcl "info_dialog {The test will be continued when the Spectrum window has been closed}"
	etcl "$image spectrum"
	if [testcancel] {return}
	while {![etcl "winfo exists $image.spectrum" ]} {
	    puts "Waiting for spectrum window to come up"
	    if [testcancel] {return}
	    after 1000
	    $itk_component(cuts) flash
	}
	etcl "wm geometry $image.spectrum +0+0"
	while {[etcl "if \!\[winfo exists $image.spectrum\] {return 0}; winfo viewable $image.spectrum" ]} {
	    puts "Waiting until spectrum window is closed"
	    if [testcancel] {return}
	    after 1000
	    $itk_component(cuts) flash
	}
    }

    # pick window

    public method pick {} {
	etcl "info_dialog {The test will be continued when the Pick Object window has been closed}"
	etcl "$image pick_dialog"
	etcl "wm geometry $image.pick +0+0"
	while {[etcl "if \!\[winfo exists $image.pick\] {return 0};winfo viewable $image.pick" ]} {
	    puts "Waiting until Pick Objec window is closed"
	    if [testcancel] {return}
	    after 1000
	    $itk_component(pick) flash
	}
    }

    # pixel table

    public method pixtab {} {
	etcl "$image pixel_table 5 5; update; wm geometry $image.pixtable +0+0"
    }

    # Performance test

    public method perft {} {
	if [testcancel] {return}
	set wper $image.perf
	etcl "$itk_option(-rtd) config -debug 1 -interval 500"
	etcl "$itk_option(-rtd) attach_camera; update"
	etcl "$image perftest; update; wm geometry $wper +0+0"
	if [testcancel] {return}
	foreach units {2 1 0} {
	    etcl "$wper set_units $units"
	    after 5000
	    if [testcancel] {return}
	    $itk_component(perft) flash
	}
	etcl "$wper cancel"
	etcl "$itk_option(-rtd) detach_camera; update"
    }

    # Rapid frame

    public method rapid {} {
	if [testcancel] {return}
	set wrap $image.rapid
	etcl "$image rapid_frame 1; update; wm geometry $wrap +0+0"
	etcl "$itk_option(-rtd) config -debug 1 -interval 200"
	etcl "$itk_option(-rtd) attach_camera; update"
	if [testcancel] {return}
	after 10000
	etcl "$itk_option(-rtd) detach_camera; update"
	etcl "destroy $wrap"
    }

    # recorder test

    public method record {} {
	set wrec $image.rec
	if [testcancel] {return}
	etcl "$image record rtdtest; update; wm geometry $wrec +0+0"
	if [testcancel] {return}
	etcl "$wrec set_globals {protect direction} 0"
	etcl "$itk_option(-rtd) config -debug 1 -interval 500"
	etcl "$itk_option(-rtd) attach_camera; update"
	# set default speed
	etcl "$wrec set_globals pbspeed 2"
	# start recording for 5 sec
	etcl "\[$wrec component record\] invoke"
	after 5000
	$itk_component(record) flash
	if [testcancel] {return}
	etcl "\[$wrec component stop\] invoke"
	etcl "\[$wrec component stop\] invoke"
	# set cycle mode
	etcl "$wrec set_globals cmode 1"
	# play slow, fast and real-time for 5 sec
	foreach el {2 1 0} {
	    etcl "$wrec set_globals pbspeed $el"
	    etcl "\[$wrec component play\] invoke"
	    after 5000
	    $itk_component(record) flash
	    if [testcancel] {return}
	    etcl "\[$wrec component stop\] invoke"
	}
	# reverse and play real-time
	etcl "$wrec set_globals pbspeed 2"
	etcl "\[$wrec component direction\] invoke"
	etcl "\[$wrec component play\] invoke"
	after 5000
	if [testcancel] {return}
	etcl "\[$wrec component stop\] invoke"
	etcl "$wrec close"
    }

    #  FITS header

    public method fitsh {} {
	if [testcancel] {return}
	etcl "$image view_fits_header"
    }

    # cut level window

    public method cut {} {
	etcl "\[$image component info\] cut_level_dialog; update"
	if [testcancel] {return}
	set cut [etcl "$image component info"].cut
	etcl "\[\[$image component info\] component low\] config -value 0"
	etcl "\[\[$image component info\] component high\] config -value 100"
	etcl "\[\[$image component info\] set_cut_levels; update"
	if [testcancel] {return}
        etcl "\[$cut component median\] invoke; update"
	if [testcancel] {return}
        etcl "\[$cut component reset\] invoke; update"
	if [testcancel] {return}
        etcl "\[$cut component set\] invoke; update"
	if [testcancel] {return}
	etcl "\[$cut component percent\] config -value 95"
	etcl "$cut set_by_percent 95; update"
        etcl "\[$cut component close\] invoke"
    }

    # zoom test

    public method zoom {} {
	set trans [etcl "$image component info component trans" ]
	set larger [etcl "$trans component larger" ]
	set i 9
	while {$i} {
	    if [testcancel] {return}
	    etcl "$larger invoke; update idletasks"
	    incr i -1
	    $itk_component(zoom) flash
	}
	etcl "$image scale 1 1"
	set smaller [etcl "$trans component smaller" ]
	set i 4
	while {$i} {
	    if [testcancel] {return}
	    etcl "$smaller invoke; update idletasks"
	    incr i -1
	    $itk_component(zoom) flash
	}
	etcl "$image scale 1 1"
    }

    # color test

    public method colors {} {
	etcl "$image set_colors; update"
	if [testcancel] {return}
	set colors .colors
        etcl "\[$colors component colormaps\] set_choice green"
        etcl "\[$colors component itts\] set_choice expo"
        etcl "\[$colors component scale\] config -value Logarithmic"
        etcl "\[$colors component allocated\] config -value 40"
        etcl "\[$colors component apply\] invoke"
	if [testcancel] {return}
	after 500
	etcl "$colors set_defaults"
        etcl "\[$colors component close\] invoke"
    }

    # complete test

    public method testall {} {
	foreach el [set cmds_] {
	    if [testcancel] {return}
	    $itk_component($el) config -state normal
	    $itk_component($el) flash
	    $itk_component($el) invoke
	}
	$itk_component(testall) flash
	puts "Test finished."
    }

    # cancel test procedure

    public method cancel {{val 1}} {
	set cancel_ $val
	if $val {
	    $itk_component(cancel) config -relief sunken
	} else {
	    $itk_component(cancel) config -relief raised
	}
	set ids [after info]
	foreach el [set $ids] {
	    catch {after cancel $el}
	}
	testcancel
    }

    # check if test was cancelled

    public method testcancel {} {
	update
	if $cancel_ {
	    after 500 "$this cancel 0"
	    return 1
	}
	return 0
    }

    # execute tcl command

    public method etcl {cmd} {
	if {$verbose_} {
	    puts "Tcl command: $cmd"
	}
	set ret [send_to_rtd $rtd_fd remotetcl $cmd]
	if {$verbose_} {
	    puts "Reply: $ret"
	}
	return $ret
    }

    # -- public vars --

    # file in images directory to load after startup
    itk_option define -file file File ngc1275.gzfits

    # widget name of Rtd
    itk_option define -rtd rtd Rtd .rtd

    # -- protected vars --
    
    # file descriptor returned by connect_to_rtd
    protected variable rtd_fd
    
    # widget name of Rtd main Image
    protected variable image
    
    # action cancelled flag
    protected variable cancel_ 0
    
    # verbose mode
    protected variable verbose_ 0
    
    # list of available commands
    protected variable cmds_
}


# connect to a running Rtd process and return the file descriptor
# for the connection socket.

proc connect_to_rtd {} {
    global env
    # get the hostname and port info from the file ~/.rtd-remote,
    # which is created by rtdimage when the remote subcommand is used
    if {[catch {set fd [open $env(HOME)/.rtd-remote]} msg]} {
	puts "can't open ~/.rtd-remote: make sure rtd is running: $msg"
	return 0
    }

    lassign [read $fd] pid host port
    close $fd

    if {[catch {exec kill -0 $pid} msg]} {
	puts "could it be that rtd is not running? ($msg)"
	return 0
    }

    set fd [server_connect -nobuf $host $port]
    return $fd
}


# send the command to rtd and return the results or generate an error

proc send_to_rtd {rtd_fd args} {
    puts $rtd_fd $args
    lassign [gets $rtd_fd] status length
    set result {}
    if {$length > 0} {
	set result [read $rtd_fd $length]
    }
    if {$status != 0} {
#	error $result
    }
    return $result
}

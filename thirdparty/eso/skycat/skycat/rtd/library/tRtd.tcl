# E.S.O. - VLT project 
# "@(#) $Id: tRtd.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# tRtd.tcl - itcl widget for starting tRtd
#
# This class starts the camera test task tRtd with user defined options.
# If the name of the testprogram is not tRtd then the testprogram is
# started in a backwards compatible way.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# pbiereic        01/03/01   Created

itk::usual tRtd {}

itcl::class rtd::tRtd {
    inherit util::TopLevelWidget
    
    constructor {args} {
	eval itk_initialize $args
	wm protocol $w_ WM_DELETE_WINDOW [code $this close]
	wm title $w_ "tRtd Control"
    }

    destructor {
	if { $tRtdPid_ > 0 } {
	    catch { kill $tRtdPid_ }
	}
    }

    protected method init {} {
	# compatible to previous versions (maybe this option was never used)
	if { "$itk_option(-testprog)" != "$tRtd_" } {
	    wm withdraw $w_
	    if { $tRtdPid_ > 0} {
		catch {kill $tRtdPid_}
	    }
	    if {[catch {set tRtdPid \
		    [exec $itk_option(-testprog) \
		    -v $itk_option(-verbose) \
		    -t $itk_option(-interval) &]} msg]} {
		error_dialog "error starting $itk_option(-testprog): $msg"
	    }
	    return
	}
	make_layout
	start
    }

    protected method make_layout {} {
	global ::$rtdimage_

	set optf [frame $w_.optf -borderwidth 2 -relief groove]
	pack $optf -fill both -expand 1

	foreach {opt text validate default} { \
		v "Verbose flag"                      numeric      0 \
		c "Camera name"                       alphanumeric rtdtest \
		t "Update interval in msecs"          numeric      500 \
		W "Main frame width"                  numeric      255 \
		H "Main frame height"                 numeric      255 \
		x "Rapid frame start x"               numeric      0 \
		y "Rapid frame start y"               numeric      0 \
		w "Rapid frame width (N/A)"           numeric      255 \
		h "Rapid frame height (N/A)"          numeric      255 \
		f "Rapid frame Id"                    numeric      0 \
		b "#of shared memory buffers"         numeric      2 \
		l "Use semaphore locking"             numeric      1 \
		I "File ngc1275.fits. Directory:"     none ""} {

	    set wdg $optf.tRtd$opt
	    set var ${rtdimage_}(tRtd$opt)
	    itk_component add $opt {
		LabelValue $wdg \
			-textvariable $var \
			-text $text \
			-relief groove \
			-justify right \
			-state normal \
			-valuewidth 10 \
			-labelwidth 22 \
			-validate $validate \
			-command [code $this start] \
			-anchor w		
	    } {
		keep -state
	    }

	    lappend vars_ "-$opt [$wdg cget -textvariable]"
	    set $var $default

	    [$wdg component entry] config -state normal \
		    -relief sunken
	    pack $wdg -fill both -expand 1
	}

	set butf [frame $w_.butf -borderwidth 2 -relief groove]
	pack $butf -fill both -expand 1

	foreach {but} {start stop rapid default close} {
	    itk_component add $but {
		button $butf.$but -text [string toupper $but] \
			-command [code $this $but] -padx 1
	    } {
		keep -state
	    }
	    pack $butf.$but -fill none -expand 1 -side left -ipadx 2 -padx 2
	}
    }

    # start process tRtd with options. Before make sure that the previous
    # process has terminated.

    public method start {args} {
	stop
	if {$tRtdPid_ > 0 } {
	    after 100 [code $this start]
	    return
	}

	global ::$rtdimage_
	foreach el $vars_ {
	    if {! [lempty [set [lindex $el 1]]]} {
		append opts "[lindex $el 0] [set [lindex $el 1]] "
	    }
	}
	# puts $opts
	$rtdimage attach_camera $itk_option(-camera)
	if { [set ${rtdimage_}(tRtdv)] } {
	    set tRtdPid_ [eval exec xterm -e $itk_option(-testprog) $opts &]
	} else {
	    set tRtdPid_ [eval exec $itk_option(-testprog) $opts &]
	}
    }

    public method stop {args} {
	if {"[cget -state]" == "disabled"} { return }

	$rtdimage detach_camera
	if { $tRtdPid_ > 0 } {
	    catch { kill $tRtdPid_ }
	    config -state disabled
	    tRtdWait
	}
    }

    public method tRtdWait {args} {
	if { [catch {wait -nohang $tRtdPid_} msg]} {
	    config -state normal
	    set tRtdPid_ -1
	} else {
	    update
	    after 100 [code $this tRtdWait]
	}
    }

    public method default {args} {
	stop
	destroy $w_.optf $w_.butf
	make_layout
    }

    public method rapid {args} {
	stop
	$rtdimage config -rapid_frame_command [code $this rapid_frame_command]
	$rtdimage rapid_frame 1
    }

    public method rapid_frame_command {f name op x y w h} {
	global ::$rtdimage_
	stop
	if {"$op" == "delete"} { return }
	set y [expr {[$rtdimage_ height] - $y - $h}]
	$rtdimage_ convert dist $x $y canvas x y image
	$rtdimage_ convert dist $w $h canvas w h image
	if {$x < 0} { set x 0 }
	if {$y < 0} { set y 0 }
	foreach el "f x y w h" { 
	    set ${rtdimage_}(tRtd$el) [set $el]
	}
	start
    }

    public method close {} {
	stop
	# compatible to previous versions:
	if { "$itk_option(-testprog)" != "$tRtd_" } {
	    return
	}
	wm withdraw $w_
    }
    
    # -- public variables (also program options) -- 

    # camera name
    itk_option define -camera camera Camera {}

    # Main Rtd image
    itk_option define -rtdimage rtdimage Rtdmage {} {
	if {[winfo exists [cget -rtdimage]]} {
	    set rtdimage  [cget -rtdimage]
	    set rtdimage_ [$rtdimage get_image]
	}
    }

    # for testing: name of test program used to generate real-time updates
    itk_option define -testprog testProg TestProg {}

    # default interval between updates in ms
    itk_option define -interval interval Interval {}

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0}

    itk_option define -buttons buttons Buttons {start stop cancel}

    itk_option define -state state State {normal}

    # -- protected variables -- 

    # pathname of main image
    protected variable rtdimage

    # name of main image object
    protected variable rtdimage_

    # pid of test prog used to generate frames (debug)
    protected variable tRtdPid_ -1

    # default name of test program
    protected variable tRtd_ "tRtd"

    # variables used by entries
    protected variable vars_

}

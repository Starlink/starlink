#******************************************************************************
# E.S.O. - VLT project
# 
# "@(#) $Id: RtdServerTool.tcl,v 1.13 1998/10/28 17:42:31 abrighto Exp $"
#
# RtdServerTool.tcl - class for controlling the realtime image server
#                     simulation
#
# who         when      what
# --------   --------   ----------------------------------------------
# T.Herlin   17/05/95   created
# A.Brighton 20/05/95   modified for Tk4.0
# A.Brighton 02/07/95   modified for rapid frame support
# T.Herlin   06/12/95   added RTD_SERVER_PORT environment
# A.Brighton 07/05/96   port to itcl/itk 2.0
# D.Hopkinson 3/12/96   call to cleanup shared memory on exit
# P.Biereichel 25/07/97 Semaphore lock added

itk::usual RtdServerTool {}

# The RtdServerTool [incr Tk] widget class, a subclass of
# TopLevelWidget, is used to manage a simulation of real-time
# image updates by communicating with the rtdServer and offering
# a Tcl level interface to it.  A toplevel window is created
# with widgets for loading a FITS file for the simulation,
# setting a timer for image updates and starting and stopping
# the simulation.
#
# Note: the simulation done with this class does not work for rapid
# frames.

itcl::class rtd::RtdServerTool {
    inherit util::TopLevelWidget 

    # constructor
    constructor {args} {
	eval itk_initialize $args

        global ::env ::sec ::msec ::imageFile ::lock

	# working varables
	set sec 0
	set msec 0
	set lock 1

	#
	# Widget layout
	#
	# START
	set file       [frame $w_.file -relief groove -borderwidth 2]
	set timer      [frame $w_.timer]
        set action     [frame $w_.action]
	# file frame
	label          $file.lbl   -text "Image File:"
	set            imageFile $itk_option(-fits_file)
	entry          $file.f     -textvariable imageFile -width 35 \
		                   -relief sunken
	bind           $file.f <Return> [code $this loadImage]
	button         $file.load  -text "Load..." -command [code $this chooseFile]
	pack           $file.lbl $file.f $file.load -padx 2 -pady 2 -side left

	# timer frame
        scale          $timer.sec   -label "Seconds"  -orient horizontal -to 20 -variable sec
        $timer.sec set 1
	scale          $timer.usec  -label "Millisec" -orient horizontal -to 999 -variable msec
        $timer.usec set 100
	button         $timer.set  -text "Set Timer"  -command [code $this timerSet]
	button	       $timer.lock -text "Unlock" -command [code $this lock] -width 6
	pack           $timer.sec $timer.usec $timer.set $timer.lock\
		         -side left -fill x -expand yes -padx 2 -pady 2
	# action frame
	button         $action.start -text "Start" -command [code $this simStart]
	button         $action.stop  -text "Stop"  -command [code $this simStop]
	button         $action.exit  -text "Close" -command [code $this close]
	pack           $action.start $action.stop $action.exit \
		         -side left -fill x  -expand yes -padx 2 -pady 2
        
	pack           $file $timer $action -fill x -padx 2 -pady 2
	#
	# END Widget layout

	#
        # BINDINGS
	#
	
	# connect to rtdServer
	if {[info exists env(RTD_SERVER_PORT)]} {
	    set portNo $env(RTD_SERVER_PORT)
	} else {
	    set portNo 5555
	}
        if {[catch {set flist [server_connect -twoids [exec uname -n] $portNo]} msg]} {
	    update idletasks
	    error_dialog "error connecting to rtdServer: $msg"
	    exit
	}
	set readFd  [lindex $flist 0]
	set writeFd [lindex $flist 1]

	fileevent $readFd readable [code $this readInput]

        puts " Note: this demo does not currently support rapid frames.\n\
              Start rtd with the debug option: 'rtd -debug 1' and select\n\
              'Attach Camera' from the Real-time menu for an alternative."
    }    

    
    # This method is called after the constructors have completed.

    protected method init {} {
        global ::imageFile
	# check that connection to rtdServer was ok
	if {"$writeFd" == ""} {
	    after 0 "wm withdraw $w_"
	    return
	}
        set imageFile ../images/ngc1275.fits
        if {[loadImage 0] != 0} {
	    set imageFile ""
	}
    }

    
    # This method is called whenever there is data to read on the rtdServer socket.
    # This is used to determine when the rtdServer has died.

    protected method readInput { } { 
	set inp "test"
	set inp [gets $readFd]
	puts "rtdServer: $inp"
	puts "The real-time display server process died !"
	exit
    }

    
    # Display the a file browser and get the name of a FITS file to load

    protected method chooseFile {} {
	global ::imageFile
	set imageFile [filename_dialog [pwd] * $w_]
	if {"$imageFile" != ""} {
	    loadImage
	}
    }


    # Specify an image to use for the simulation.

    public method loadImage {{errdiag 1}} { 
	global ::imageFile
	if {[file exist $imageFile] == 1} {
	    set dirname [file dirname $imageFile]
	    if { [string index $dirname 0] == "."} {
		set simFile "[pwd]/$imageFile"
	    } else {
		set simFile "$imageFile"
	    }
	} else {
	    if {$errdiag} {
		error_dialog "invalid filename specified: $imageFile" $w_
	    }
	    return 1
	}
	set rtdServerLoad "RTDTCL LOAD"
	puts $writeFd "$rtdServerLoad $simFile"
	flush $writeFd
	return 0
    }

    
    # start the simulation.

    public method simStart { } { 
	global ::imageFile
	if {"$imageFile" == ""} {
	    error_dialog "Load a FITS image first before starting"
	    return
	}
	if {"$rtd_fd" == ""} {
	    if {[catch {connect_to_rtd} msg]} {
		error_dialog "$msg"
		return
	    }
	    set rtd_fd $msg
	    if {$rtd_fd == 0} {
		error_dialog "Cannot connect to RTD: $itk_option(-rtd). Check that it is running."
		set rtd_fd ""
		return
	    }
	}
        etcl "catch {\[$itk_option(-rtd) component image\].rec chooseCamera RTDSIMULATOR 1}"
        etcl "$itk_option(-rtd) config -camera RTDSIMULATOR -debug 0"
        etcl "$itk_option(-rtd) attach_camera; update"

	set rtdSimStart "RTDTCL SIMSTART"
	puts $writeFd "$rtdSimStart"
	flush $writeFd
	timerSet
    }

    
    # stop the simulation.

    public method simStop { } {
	set rtdSimStop "RTDTCL SIMSTOP"
	puts $writeFd "$rtdSimStop"
	flush $writeFd
    }
 
    
    # set the simulation timer.

    public method timerSet { } { 
	global ::sec ::msec
	set rtdTimerSet "RTDTCL TIMERSET"
	puts $writeFd "$rtdTimerSet $sec [expr $msec * 1000] "
	flush $writeFd
    }

    
    # This method is called when the "Lock" button is pressed.

    public method lock { } {
	global ::lock
	if {$lock == 1} {
	    set lock 0
	    $w_.timer.lock configure -text "Lock"
	} else {
	    set lock 1
	    $w_.timer.lock configure -text "Unlock"
	}
	set rtdLock "RTDTCL LOCK"
	puts $writeFd "$rtdLock"
	flush $writeFd
    }


    # initialize or modify a rapid frame.
    # The args are: the frame id (a number), the x,y coords of the
    # frame and the width and height.

    public method rapidFrame { id x y w h } { 
	puts $writeFd "RTDTCL RAPIDFRAME $id $x $y $w $h"

	flush $writeFd
    }

    
    # Remove the rapid frame given by the id.

    public method removeRapidFrame { id } { 
	puts $writeFd "RTDTCL RMRAPID $id"
	flush $writeFd
    }

    
    # Stop all actions and delete the window.

    public method close { } {
	puts $writeFd "RTDTCL CLEANUP"
	flush $writeFd
	quit
    }

    # Send the given command to rtd to be evaluated and return the result.

    public method etcl {cmd} {
        set ret [send_to_rtd $rtd_fd remotetcl $cmd]
        return $ret
    }

    # -- public vars --

    # widget name of Rtd
    itk_option define -rtd rtd Rtd .rtd
    
    # FITS file to use for the simulation.
    itk_option define -fits_file fits_file Fits_file {test.gzfits}
    
    #
    # protected variables
    #

    # read file descriptor for rtdServer
    protected variable readFd {}

    # write file descriptor for rtdServer
    protected variable writeFd {}

    # file descriptor returned by connect_to_rtd
    protected variable rtd_fd {}
}


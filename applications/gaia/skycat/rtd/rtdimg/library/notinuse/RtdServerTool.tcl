#******************************************************************************
# E.S.O. - VLT project
# 
# "@(#) $Id: RtdServerTool.tcl,v 1.2 1997/09/17 13:39:44 abrighto Exp $"
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

itk::usual RtdServerTool {}

class rtd::RtdServerTool {
    inherit TopLevelWidget 

    # constructor
    constructor {args} {
	eval itk_initialize $args

        global ::env ::sec ::msec ::imageFile

	# working varables
	set sec 2
	set msec 0
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
	$timer.sec set 2
	scale          $timer.usec  -label "Millisec" -orient horizontal -to 999 -variable msec
	button         $timer.set  -text "Set Timer"  -command [code $this timerSet]
	pack           $timer.sec  $timer.usec $timer.set \
		         -side left -fill x -expand yes -padx 2 -pady 2
	# action frame
	button         $action.start -text "Start" -command [code $this simStart]
	button         $action.stop  -text "Stop"  -command [code $this simStop]
	button         $action.exit  -text "Close" -command [code $this quit]
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
	    destroy $w_
	    error_dialog "error connecting to RTD server: $msg" $w_
	    return
	}
	set readFd  [lindex $flist 0]
	set writeFd [lindex $flist 1]

	fileevent $readFd readable [code $this readInput]
	
	# allan: 23.6.96: rapid frames have separate shared memory area starting
	# at 0,0. There is a test prog (../test/tRtd) that tests this. If you
	# specify -debug 1 to rtd, it will start tRtd with the correct options
	# for the rapid frame size, etc...
	info_dialog "Note: this demo does not currently support rapid frames. \
                     Start rtd with the debug option: 'rtd -debug 1' and select \
                     'Attach Camera' from the Real-time menu for an alternative."

    }    


    method init {} {
        global ::imageFile
        set imageFile ../images/ngc1275.fits
        loadImage
    }

    
    method readInput { } { 
	set inp "test"
	set inp [gets $readFd]
	puts "rtdServer: $inp"
	puts "The real-time display server process died !"
	exit
    }

    
    # Display the a file browser and get the name of a FITS file to load
    # (allan: 20.5.96)

    method chooseFile {} {
	global ::imageFile
	set imageFile [filename_dialog [pwd] * $w_]
	if {"$imageFile" != ""} {
	    loadImage
	}
    }


    method loadImage { } { 
	global ::imageFile
	if {[file exist $imageFile] == 1} {
	    set dirname [file dirname $imageFile]
	    if { [string index $dirname 0] == "."} {
		set simFile "[pwd]/$imageFile"
	    } else {
		set simFile "$imageFile"
	    }
	} else {
		error_dialog "invalid filename specified: $imageFile" $w_
		return
	}
	set rtdServerLoad "RTDTCL LOAD"
	puts $writeFd "$rtdServerLoad $simFile"
	flush $writeFd
    }

    method simStart { } { 
	set rtdSimStart "RTDTCL SIMSTART"
	puts $writeFd "$rtdSimStart"
	flush $writeFd
    }

    method simStop { } { 
	set rtdSimStop "RTDTCL SIMSTOP"
	puts $writeFd "$rtdSimStop"
	flush $writeFd
    }
 
    method timerSet { } { 
	global ::sec ::msec
	set rtdTimerSet "RTDTCL TIMERSET"
	puts $writeFd "$rtdTimerSet $sec [expr $msec * 1000] "
	flush $writeFd
    }


    # initialize or modify a rapid frame (allan)
    # args are: the frame id (a number), the x,y coords of the
    # frame and the width and height.

    method rapidFrame { id x y w h } { 
	puts $writeFd "RTDTCL RAPIDFRAME $id $x $y $w $h"

	flush $writeFd
    }

    method removeRapidFrame { id } { 
	puts $writeFd "RTDTCL RMRAPID $id"
	flush $writeFd
    }

    #--------------------------------------------------------------------
    #  variables 
    #--------------------------------------------------------------------
    
    
    itk_option define -fits_file fits_file Fits_file {test.fits}
    
    #
    # protected variables
    #
    protected variable readFd {}
    protected variable writeFd {}
}


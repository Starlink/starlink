#******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: rtdServerTool.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $" 
#
# rtdServerTool.tcl
#
# who         when      what
# --------   --------   ----------------------------------------------
# T.Herlin   17/05/95   created
#
#
#************************************************************************
#   NAME
#
#     rtdServerTool
# 
#   SYNOPSIS
#
#     rtdServerTool  <widget-name>
# 
#   DESCRIPTION
#
#   EXAMPLES
#
#   SEE ALSO
#
#
#------------------------------------------------------------------------
#/*

#------------------------------------------------------------------------
#*# class: rtdServerTool

itcl_class rtdServerTool {

    inherit FrameWidget 

    #*# constructor
    constructor { config } {

        global env

	FrameWidget::constructor

	# working varables
        set w   $this
	set sec 2
	set msec 0
	#
	# Widget layout
	#
	# START
	set file       [frame $w.file -relief groove -bd 2]
	set timer      [frame $w.timer]
        set action     [frame $w.action]
	# file frame
	label          $file.lbl   -text "Image File:"
	entry          $file.f     -textvariable imageFile -width 35 \
		                   -relief sunken
	button         $file.load  -text Load -command "$this loadImage"
	pack           $file.lbl $file.f $file.load -padx 2 -pady 2 -side left

	# timer frame
        scale          $timer.sec   -label "Seconds"  -orient horizontal -to 20 -command "set sec"
	$timer.sec set 2
	scale          $timer.usec  -label "Millisec" -orient horizontal -to 999 -command "set msec"
	button         $timer.set  -text "Set Timer"  -command "$this timerSet"
	pack           $timer.sec  $timer.usec $timer.set \
		         -side left -fill x -expand yes -padx 2 -pady 2
	# action frame
	button         $action.start -text "Start" -command "$this simStart"
	button         $action.stop  -text "Stop"  -command "$this simStop"
	button         $action.exit  -text "Dismiss" -command "$this quit"
	pack           $action.start $action.stop $action.exit \
		         -side left -fill x  -expand yes -padx 2 -pady 2
        
	pack           $file $timer $action -fill x -padx 2 -pady 2
	#
	# END Widget layout

	#
        # BINDINGS
	#

	# connect to rtdServer
        set flist [server_open $env(HOST) 5555]
	set readFd  [lindex $flist 0]
	set writeFd [lindex $flist 1]

	addinput $readFd "$this readInput"

	#  Explicitly handle config's that may have been ignored earlier
        set initialized 1

                
        foreach attr $config {
            configure -$attr [set $attr]
        }
	
	#
        # SET DEFAULTS 
	#
	
    }    

    
    #--------------------------------------------------------------------
    #*# method: configure

    method configure { config } { }

    method readInput { } { 
	set inp "test"
	set inp [gets $readFd]
	puts "rtdServer: $inp"
	exit
    }

    method loadImage { } { 
	global imageFile
	set rtdServerLoad "RTDTCL LOAD"
	puts $writeFd "$rtdServerLoad $imageFile"
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
	global sec msec
	set rtdTimerSet "RTDTCL TIMERSET"
	puts $writeFd "$rtdTimerSet $sec [expr $msec * 1000] "
	flush $writeFd
    }
    #--------------------------------------------------------------------
    #  variables 
    #--------------------------------------------------------------------
    
    # handle width & height
    
    public width {} {
	
	if {$initialized == 0 } {return} 

	if { $width != "" } { $rtd configure -width $width }
    }
    
    #
    # TOO LONG LIST of protected variables
    #
    protected initialized  0  
    protected readFd
    protected writeFd


}

rtdServerTool .server 
pack .server


#!/bin/sh
#\
exec rtdimage_wish "$0" ${1+"$@"}

# E.S.O. - VLT project 
# "@(#) $Id: tRemote.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# tRemote.tcl - automatic test procedure for Rtd via remote interface
#
# who             when       what
# --------------  ---------  ----------------------------------------
# P. Biereichel   11/08/99   Created

# debug flag for development
set debug 0

proc feedback { args } {
    global debug
    if {$debug != 0} {
	puts $args
    }
}

proc startRtd {} {
    feedback "starting rtd ..."
    return [exec rtd -shm_data 1 -shm_header 1 -debug 1 -rtd_geometry -0+0 \
	    -rtd_title {RTD TEST ** RTD TEST ** RTD TEST} &]
}

proc connectToRtd {} {
    global ::rtd_fd
    while {1} {
	feedback "connecting to Rtd..."
	# open the connection
	set rtd_fd [connect_to_rtd]
	if {$rtd_fd > 0} { break }
	sleep 2
    }
    feedback "connected to rtd."
}

proc sendTclCmd {cmd} {
    global ::rtd_fd
    # feedback "sending rtd tcl command: $cmd"
    set ret [etcl $rtd_fd $cmd]
    # feedback "received reply: $ret\n"
    return $ret
}

proc sendRtdCmd {cmd} {
    global ::rtd_fd
    # feedback "sending rtd subimage command: $cmd"
    set ret [send_to_rtd $rtd_fd $cmd]
    # feedback "received reply: $ret\n"
    return $ret
}

################################################################################
# Test some rtd subcommands
################################################################################

proc testSubCommands {rtd image_path file} {
    sendRtdCmd "clear"
    sendRtdCmd "config -file $file"

    sendRtdCmd "alloccolors 40"
    sendRtdCmd "autocut"

    # biasimage subcommand
    sendRtdCmd "biasimage file $file 1"
    sendRtdCmd "biasimage select 1"
    sendRtdCmd "biasimage on"
    sendRtdCmd "autocut"
    sendRtdCmd "biasimage off"
    sendRtdCmd "biasimage clear 1"
    sendRtdCmd "autocut"

    sendRtdCmd "bitpix"

    # cmap subcommand
    for {set i 1} {$i < 5} {incr i} {
	sendRtdCmd "cmap rotate $i"
	sendRtdCmd "cmap shift $i"
    }
    sendRtdCmd "cmap reset"

    # colorscale subcommand
    foreach col "log sqrt histeq linear" {
	sendRtdCmd "colorscale $col"
    }

    #sendRtdCmd "convert"
    sendRtdCmd "cut 0 1000"
    sendRtdCmd "autocut"

    sendRtdCmd "dispheight"
    sendRtdCmd "dispwidth"

    # dump subcommand
    set f /tmp/rtd.fits
    sendRtdCmd "dump $f 0 0 50 50"
    sendRtdCmd "config -file $f"
    sendRtdCmd "autocut"
    sendRtdCmd "config -file $file"

    sendRtdCmd "fits get"
    sendRtdCmd "flip XY T"
    sendRtdCmd "flip XY F"
    sendRtdCmd "frameid"
    sendRtdCmd "get 10 10 image 2 2"
    #sendRtdCmd "graphdist"
    sendRtdCmd "height"
    sendRtdCmd "isclear"

    # itt subcommand
    sendRtdCmd "itt list"
    sendRtdCmd "itt file ramp.iasc"
    sendRtdCmd "itt scale 2"
    sendRtdCmd "itt scale 1"

    sendRtdCmd "max"
    sendRtdCmd "mband 10 10 60 60 image 1"
    sendRtdCmd "min"
    #sendRtdCmd "mmap"

    sendRtdCmd "motionevent 0"
    sendRtdCmd "motionevent 1"

    sendRtdCmd "object"
    #sendRtdCmd "pan"

    #sendRtdCmd "perftest"

    sendRtdCmd "pixtab start 3 3"
    sendRtdCmd "pixtab stop"

    #sendRtdCmd "preview 1"
    #sendRtdCmd "preview 0"
    #sendRtdCmd "radecbox"
    #sendRtdCmd "remote"

    sendRtdCmd "rotate 1"
    sendRtdCmd "rotate 0"

    sendRtdCmd "scale 2 2"
    sendRtdCmd "scale 1 1"

    sendRtdCmd "shm get header"
    sendRtdCmd "shm get data"

    #sendRtdCmd "spectrum"
    #sendRtdCmd "statistics" (core dumps!)
    sendRtdCmd "type"
    sendRtdCmd "update"

    #sendRtdCmd "userfreq"
    #sendRtdCmd "view"

    for {set i 1} {$i < 30} {incr i 5} {
	sendRtdCmd "warp 5 5"
    }
    sendRtdCmd "wcscenter"
    sendRtdCmd "wcsdist 10 10 50 50"
    sendRtdCmd "wcsequinox"
    sendRtdCmd "wcsheight"
    sendRtdCmd "wcsradius"
    sendRtdCmd "wcsset"
    #sendRtdCmd "wcsshift"
    sendRtdCmd "wcswidth"
    sendRtdCmd "width"
    #sendRtdCmd "zoom"
    #sendRtdCmd "zoomview"
    sendRtdCmd "clear"
}

################################################################################
# Test Rtd widgets
################################################################################

proc testWidgets {rtd image_path file} {
    set image [sendTclCmd "$rtd component image"]

    sendRtdCmd clear

    sendTclCmd "$rtd attach_camera"
    sleep 5
    sendTclCmd "$image spectrum"
    #set spectrum [sendTclCmd "$image component spectrum"]
    set spectrum $image.spectrum
    while {[sendTclCmd "winfo exists $spectrum"] == 0} {
	sleep 1
    }
    sleep 4
    sendTclCmd "$rtd detach_camera"
    sendTclCmd "$spectrum quit"

    sendTclCmd "$image inc_zoom -1"
    sleep 1
    sendTclCmd "$image inc_zoom 1"
    sleep 1

    sendRtdCmd clear
    sendTclCmd "$image config -shm_data 0 -shm_header 0"

    feedback "please load file ngc1275.fits"
    feedback "waiting until image was loaded..."
    sendTclCmd "$image open $image_path *.*fits*"
    while {[sendRtdCmd isclear] == 1} {
	sleep 2
	feedback "select 'file', 'open...' and load an image"
    }

    feedback "please pick an object"
    sendTclCmd "$image pick_dialog {global ::pickVar; set pickVar}"
    while {[sendTclCmd "global ::pickVar; info exists pickVar"] == 0} {
	sleep 2
    }
    sleep 2
    sendTclCmd "$image.pick close"

    feedback "please close the FITS header window"
    sendTclCmd "$image view_fits_header"

    sendTclCmd "$image rotate 1"
    sendTclCmd "$image flip x 1"

    sendTclCmd "$image wcs_info_dialog"
    sendTclCmd "$image set_colors"
    sendTclCmd "$image show_toolbox"
    sendTclCmd "$image pixel_table 5 5"
    sleep 5

    sendTclCmd "global ::$rtd.hide_windows; set $rtd.hide_windows 1; \
	    $rtd hide_windows $rtd.hide_windows"
    sendTclCmd "global ::$rtd.hide_control_panel; set $rtd.hide_control_panel 1; \
	    $image hide_control_panel $rtd.hide_control_panel"

    #sendTclCmd "$image config -file $image_path/img_ext.fits"
    #sleep 2
}


################################################################################
# Start Rtd tests
################################################################################

set execDir [file dirname [info nameofexecutable]]
lappend auto_path $execDir/../library $execDir/../lib/rtd
set rtd .rtd1

set rtdPid [startRtd]

# connect to rtd
connectToRtd

# load a file
set rtd_library [sendTclCmd {global ::rtd_library; set rtd_library}]
set image_path "$rtd_library/../images"
if {! [file isdirectory $image_path]} {
    set image_path $rtd_library/images
}
set file "$image_path/ngc1275.fits"
if {! [file isfile $file]} {
    feedback "File $file not found. Cannot execute this test."
    exit 1
}

# test some rtd subcommands (see RtdImage.C)
feedback "\n\n\nTESTING RTD SUBCOMMANDS\n\n"
testSubCommands $rtd $image_path $file

# test the widget library
feedback "\n\n\nTESTING RTD WIDGETS\n\n"
testWidgets $rtd $image_path $file

feedback "\n\n\nTEST FINISHED SUCCESSFULLY\n"
catch "kill $rtdPid"
exit

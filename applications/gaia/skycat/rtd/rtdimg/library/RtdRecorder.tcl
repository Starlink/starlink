#******************************************************************************
# E.S.O. - VLT project
#
# RtdRecorderTool.tcl - class for recording or playback of images.
#
# who          when      what
# --------     --------  ----------------------------------------------
# D.Hopkinson  19/02/97  Created
# P.Biereichel 15/07/97  Bugs fixed - revised

itk::usual RtdRecorderTool {}

# RtdRecorderTool is a class for controlling the recording or playing
# back of images.
#
# This widget doubles as a recorder and playback tool. As a recorder,
# it acts as an effective RTD, receiving images from the camera named
# in the camera_ variable. As a playback, the camera becomes
# hardwired to RTDRPTOOL.

itcl::class rtd::RtdRecorderTool {
    inherit util::TopLevelWidget 

    constructor {args} {
	eval itk_initialize $args
	global ::recorder_init

	set rtdplayback_ [image create rtdplayback]
	set rtdrecorder_ [image create rtdrecorder]

	wm title $w_ "Video Record/Playback Images ($itk_option(-number))"
	wm resizable $w_ 0 0
	wm protocol $w_ WM_DELETE_WINDOW ""

	# Widget layout
	add_menubar
	make_layout
	make_shorthelp
	trace_counts
	set recorder_init 1
    }

    destructor {
	# Stop any recordings/playings before closing.
	stop

	$rtdrecorder_ close
	$rtdplayback_ close

	# Set back to the original camera before going.
	$mainwidg_ config -camera $itk_option(-server_camera)
    }

    # make the widget layout

    protected method make_layout {} {
	global ::$w_.comp ::$w_.pbspeed ::$w_.direction ::$w_.protect ::recorder_init ::$w_.cmode
	# Tk frame for status
	itk_component add status {
	    frame $w_.status -relief groove -borderwidth 2
	}

	# Tk label: "File Position:"
        itk_component add progressLabel {
            label $itk_component(status).progressLabel \
		-text "File Position:" \
		-width 11 \
		-font $itk_option(-labelfont) \
 		-anchor w
        }

	# Tk scale for progress bar
	itk_component add progressBar {
            scale $itk_component(status).progressBar \
                -orient horizontal \
                -borderwidth 2 \
		-length 100 \
		-width 10 \
                -sliderlength 0 \
		-state disabled \
		-troughcolor grey80 \
		-bg grey80 \
                -from 0 \
                -to 1 \
                -showvalue 0 \
                -highlightthickness 0
        }

	# LabelEntry(n) widget, image count
	itk_component add imagecount {
	    util::LabelEntry $itk_component(status).imagecount \
		-disabledforeground black \
		-text "Image Count:" \
		-labelfont $itk_option(-labelfont) \
		-labelwidth 12 \
		-valuewidth 8 \
		-validate integer \
		-valuefont $itk_option(-valuefont) \
		-relief sunken \
		-command [code $this gotoImage] \
		-borderwidth 2 \
		-orient horizontal 
	} {
	    keep -state
	}

	# LabelEntry(n) widget "of:"
	itk_component add ncounts {
	    util::LabelEntry $itk_component(status).ncounts \
		-text "of:" \
		-labelfont $itk_option(-labelfont) \
		-labelwidth 3 \
		-valuewidth 5 \
		-valuefont $itk_option(-valuefont) \
		-relief groove \
		-disabledforeground black \
		-state disabled \
		-borderwidth 2 \
		-orient horizontal 
	}

	# Tk frame for file name
	itk_component add fileframe {
	    frame $w_.file -relief groove -borderwidth 2
	}

	# LabelEntry(n) widget  File Name:"
	itk_component add filename {
	    util::LabelEntry $itk_component(fileframe).filename \
		-text "File Name:" \
		-value $imageFile_ \
		-labelwidth 12 \
		-justify right \
		-labelfont $itk_option(-labelfont) \
		-valuewidth 28 \
		-valuefont $itk_option(-valuefont) \
		-relief sunken \
		-borderwidth 2 \
		-command [code $this chooseFile] \
		-orient horizontal
	} {
	    keep -state
	}

	# Set the default camera
	set camera_ $itk_option(-server_camera)

	# LabelEntry(n) widget "Camera Name:"
	itk_component add cameraname {
	    util::LabelEntry $itk_component(fileframe).cameraname \
		-text "Camera Name:" \
		-labelwidth 12 \
		-command [code $this chooseCamera] \
		-labelfont $itk_option(-labelfont) \
		-valuewidth 28 \
		-valuefont $itk_option(-valuefont) \
		-relief sunken \
		-borderwidth 2 \
		-value $camera_ \
		-orient horizontal
	} {
	    keep -state
	}

	# action frame
	itk_component add action {
	    frame $w_.action -relief groove -borderwidth 2
	}

	# Tk frame pbaction1
	itk_component add pbaction1 {
	    frame $itk_component(action).pbaction1 -relief flat -borderwidth 2
	}

	# checkbutton "Reverse direction"
	itk_component add direction {
	    checkbutton $itk_component(pbaction1).direction \
		-text "Reverse direction" \
		-command [code $this stop] \
		-variable $w_.direction \
		-anchor w \
		-font $itk_option(-labelfont)
	} {
	    keep -state
	}

	# checkbutton "Protect"
	itk_component add protect {
	    checkbutton $itk_component(pbaction1).protect \
		-text "Protect" \
		-command [code $this protect] \
		-variable $w_.protect \
		-anchor e \
		-font $itk_option(-labelfont)
	} {
	    keep -state
	}
	set $w_.protect 1

	# Tk frame pbaction2
	itk_component add pbaction2 {
	    frame $itk_component(action).pbaction2 -relief raised -borderwidth 2
	}

	# Play button
	itk_component add play {
	    button $itk_component(pbaction2).play \
		-bitmap big_right \
		-command [code $this play]
	} {
	    keep -state
	}

	# Rewind button
	itk_component add rewind {
	    button $itk_component(pbaction2).rewind \
		-bitmap double_left \
		-command [code $this spool "rewind"] \
	} {
	    keep -state
	}

	# fast forward button
	itk_component add ff {
	    button $itk_component(pbaction2).ff \
		-bitmap double_right \
		-command [code $this spool "ff"]
	} {
	    keep -state
	}

	# single step button
	itk_component add single {
	    button $itk_component(pbaction2).single \
		-bitmap Right \
		-command [code $this single]
	} {
	    keep -state
	}

	# Record button
	itk_component add record {
	    button $itk_component(pbaction2).record \
		-bitmap record \
		-foreground red \
		-disabledforeground red \
		-command [code $this record]
	}

	# Stop button
	itk_component add stop {
	    button $itk_component(pbaction2).stop \
		-bitmap rect \
		-command [code $this stop]
	}

	# Set the defaults
	if {![info exists recorder_init] || $recorder_init != 1} {
	    set $w_.direction 0
	    set $w_.pbspeed 1
	    set $w_.cmode 1
	}

	# do the packing

	blt::table $itk_component(status) \
	    $itk_component(progressLabel)      1,0 -anchor w -fill x \
	    $itk_component(progressBar)        1,1 -anchor e -fill x -columnspan 2 \
	    $itk_component(imagecount)         2,0 -anchor w -fill x -columnspan 2 \
	    $itk_component(ncounts)            2,2 -anchor w -fill x

	blt::table $itk_component(fileframe) \
	    $itk_component(filename)        1,0 -anchor w -fill x \
	    $itk_component(cameraname)      2,0 -anchor w -fill x

	blt::table $itk_component(pbaction1) \
	    $itk_component(direction)   1,0 -anchor w -fill x \
	    $itk_component(protect)     1,1 -anchor e -fill x	    

	blt::table $itk_component(pbaction2) \
	    $itk_component(play)        1,0 -anchor w -fill none \
	    $itk_component(rewind)      1,1 -anchor w -fill none \
	    $itk_component(ff)          1,2 -anchor w -fill none \
	    $itk_component(single)      1,3 -anchor w -fill none \
	    $itk_component(record)      1,4 -anchor w -fill none \
	    $itk_component(stop)        1,5 -anchor w -fill none \

	pack $itk_component(status) $itk_component(fileframe) $itk_component(action) \
	     $itk_component(pbaction1) -side top -anchor w -fill x -expand 0 -padx 5 -pady 5

	pack $itk_component(pbaction2) \
	    -side top -anchor c -fill none -expand 0 -padx 5 -pady 5

    }

    # add short help text

    protected method make_shorthelp {} {
	add_short_help $itk_component(progressBar) {Displays the proportion of the maximum file size that has been recorded or proportion played back}
	add_short_help $itk_component(imagecount) {Image counter when playing back images from file}
	add_short_help $itk_component(ncounts) {Number of images in file}
	add_short_help $itk_component(filename) {Displays the name of the file to record/play. Use <Enter key> after editing.}
	add_short_help $itk_component(cameraname) {Sets the camera from which images are to be received}
	add_short_help $itk_component(direction) {Reverse the playback direction}
	add_short_help $itk_component(protect) {Set write protect/unprotect file}
	add_short_help $itk_component(play) {Playback images from file}
	add_short_help $itk_component(rewind) {Rewind image file}
	add_short_help $itk_component(ff) {Fast-forward image file}
	add_short_help $itk_component(single) {Single-step through file}
	add_short_help $itk_component(record) {Record images from camera to file}
	add_short_help $itk_component(stop) {Stop record/playback}
    }

    # Method to add the menu bar to the top of the dialogue.
    protected method add_menubar {} {
	global ::$w_.cmode ::$w_.pbspeed
	TopLevelWidget::add_menubar

	# File menu
	set m [add_menubutton File]

	add_menuitem $m command "Load File..." \
	    {Load a file for recording or playing} \
	    -command [code $this chooseFile]

	add_menuitem $m command "Exit" \
	    {Exit the application} \
	    -command [code $this close]

	# Options menu
	set m [add_menubutton "Options"]

	# Change the maximum allowed file size
	add_menuitem $m command "Maximum File Size..." \
	    {Alter the minimum possible interval between image send events} \
	    -command [code $this set_max_filesize]

	# Image cycle mode
	add_menuitem $m checkbutton \
	    "Cycle Mode" \
	    {Turn the image cycling on or off} \
	    -variable $w_.cmode -onvalue 1 -offvalue 0 \
	    -command [code $this stop]

	# Change the playback speed
	add_menuitem $m cascade "Playback Speed" \
	    {Set the speed at which to play back images} \
	    -menu [menu $m.pb]

	# NB the values given to these compression types correspond to the
	# ENUM which defines the Playback speed in RtdRPTool.h.
	$m.pb add radiobutton \
	    -label "Real Time" \
	    -command [code $this stop] \
	    -variable $w_.pbspeed \
	    -value 2

	$m.pb add radiobutton \
	    -label "Slow" \
	    -command [code $this stop] \
	    -variable $w_.pbspeed \
	    -value 0

	$m.pb add radiobutton \
	    -label "Fast" \
	    -command [code $this stop] \
	    -variable $w_.pbspeed \
	    -value 1

	$m add separator

	# Edit the window
	add_menuitem $m command "Edit Record Window..." \
	    {Edit the region of the image to record} \
	    -command [code $this edit_window]

	add_menuitem $m command "Reset Record Window" \
	    {Reset the region of the image to record to full image} \
	    -command [code $this reset_window]
    }

    # inititialize trace variables

    protected method trace_counts {} {
	set var1 $rtdrecorder_
	global ::$var1
	set var2 $rtdplayback_
	global ::$var2
	set ${var1}(COUNT) 1
	set ${var2}(COUNT) 1
	trace variable ${var1}(COUNT) w [code $this update_progress]
	trace variable ${var2}(COUNT) w [code $this update_progress]
    }

    # update progress bar and image count

    protected method update_progress {args} {
	global ::$w_.cmode ::$w_.direction
	if {$recording_} {
	    set var $rtdrecorder_
	} else {
	    set var $rtdplayback_
	}
	global ::$var
	lassign [set ${var}(COUNT)] count ncount bof eof
	if {[set $w_.direction] != 0 && !$recording_} {
	    lassign "$bof $eof" eof bof
	}
	$itk_component(imagecount) config -value $count
	$itk_component(ncounts) config -value $ncount
	set w $itk_component(progressBar)
	$w config -state normal -from 0 -to $ncount -troughcolor blue
	$w set 0
	$w config -bg grey80
	$w set $count
	$w config -state disabled
	# puts "$count,$ncount bof=$bof eof=$eof"
	if {([set $w_.cmode] == 0 && $eof != 0)} {
	    # return to interpreter. Do not send commands to a subimage!
	    after idle "$this stop"
	}
    }

    # method to set/reset cycle mode
    public method set_cycle_mode {} {
	global ::$w_.cmode
	$rtdrecorder_ cycle [set $w_.cmode]
    }

    # Reset the window to send to full image.
    public method reset_window {} {
	stop
	set subimage_ 0
	if {[winfo exists $w_.subimage]} {
	    destroy $w_.subimage
	} 
    }

    # Set the recorder properties prior to starting recording.
    public method recorder_props {} {
	global ::$w_.cmode

	$rtdrecorder_ file size $maxFile_
	$rtdrecorder_ cycle [set $w_.cmode]
	$rtdrecorder_ filename $imageFile_
	$rtdrecorder_ camera $camera_
	set attached_ 0
	if {$subimage_ == 0} {
	    $rtdrecorder_ subimage off
	} else {
	    $rtdrecorder_ subimage on $x0_ $y0_ $width_ $height_
	}
    }

    # Set the playback props prior to starting a playback.
    public method playback_props {} {
	global ::$w_.cmode ::$w_.pbspeed ::$w_.direction

	if {[set $w_.direction] == 1} {
	    set dir 0
	} else {
	    set dir 1
	}
	$rtdplayback_ filename $imageFile_
	$rtdplayback_ cycle [set $w_.cmode]

	if {[catch {$rtdplayback_ props speed [set $w_.pbspeed]} msg]} {
	    error_dialog $msg
	    return 1
	}
	$rtdplayback_ props direction $dir
	return 0
    }

    # Set file protect mode

    public method protect {} {
	global ::$w_.protect
	if {[set $w_.protect]} {
	    $itk_component(record) config -state disabled
	} else {
	    $itk_component(record) config -state normal
	}
    }

    # Edit the record window

    public method edit_window {} {
	stop
	if {[$target_image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	if {$playing_ == 1 || $spooling_ == 1} {
	    warning_dialog "Stop playback before selecting record window" $w_
	    return
	}

	if {[winfo exists $w_.subimage]} {
	    destroy $w_.subimage
	} 
 
	if {[action_dialog \
		 "Please select and drag out a region of the image with mouse button 1" \
		 $w_]} { 
	    [$target_image_ component draw] set_drawing_mode region [code $this make_rapid_frame]
	}
    }

    # create a rapid frame.

    protected method make_rapid_frame {region_id x0 y0 x1 y1} {
	set xoffset [expr int($x0)]
	set yoffset [expr int($y0)]
	set width [expr int($x1-$x0+1)]
	set height [expr int($y1-$y0+1)]
	set subimage_ 1

	RtdImageFrame $w_.subimage \
	    -target_image $target_image_ \
	    -xoffset $xoffset \
	    -yoffset $yoffset \
	    -width $width \
	    -height $height \
	    -region_id $region_id \
	    -verbose 0 \
	    -command [code $this set_subimage]
    }

    # set the values of x0_, y0_, width_ and height_ from
    # the subimage coordinates

    protected method set_subimage {frameId name op x y w h} {
	set im [$target_image_ get_image] 
	$im convert coords $x $y canvas x0_ y0_ image
	$im convert dist $w $h canvas width_ height_ image
    }

    # Set the maximum allowed file size in recording images

    public method set_max_filesize {} {
	utilReUseWidget InputDialog $w_.maxfsize \
	    -title "Maximum File Size" \
	    -text "Set maximum recording file size in Mb (currently $maxFile_ Mb)" \
	    -modal 1 \
	    -bitmap information \
	    -buttons {OK Cancel}

	set val_returned [$w_.maxfsize activate]
	if {$val_returned != ""} {
	    set maxFile_ $val_returned
	}
    }

    # Creates the dialog for choosing the file to record to or to
    # play from

    public method chooseFile {{file ""}} {
	global ::$w_.protect
	if {"$file" == ""} {
	    set file [filename_dialog [pwd] * $w_]
	    if {"$file" == ""} {
		$itk_component(filename) config -value $imageFile_
		return
	    }
	}
	set file [fits_pathname $file]
	if {[file isdir $file]} {
	    error_dialog "$file is a directory" $w_
	    return
	}
	if {[llength $file] != 0} {
	    set $w_.protect 1
	    stop
	    $rtdplayback_ reset
	    set imageFile_ $file
	}
	$itk_component(filename) config -value $imageFile_
    }

    # return 'globbed' pathname for a fits file. If the extension is
    # not set in $file then .fits is used

    public method fits_pathname {file} {
	set dirname [lindex [file dirname $file] 0]
	if {[catch {glob $dirname} dirname]} {
	    return $file
	}
	set basename [file split $file]
	set basename [lindex $basename [expr [llength $basename] -1]]
	set ext [file extension $file]
	if {"$ext" == ""} {
	    set file [file join $dirname $basename.fits]
	} else {
	    set file [file join $dirname $basename]
	}
	if {[catch {glob $file} gfile]} {
	    return $file
	}
	return $gfile
    }

    # Set camera name

    public method chooseCamera {camera {stop 0}} {
	set recflg 0
	if {($stop && ! $recording_) || ([llength $camera] != 0 && "$camera_" != "$camera")} {
	    if {$recording_} {
		set recflg 1
	    }
	    stop
	    set camera_ $camera
	    $rtdplayback_ reset
	}
	$itk_component(cameraname) config -value $camera_
	if {$recflg} {
	    # resume recording after camera name has changed
	    after idle "$this record"
	}
    }

    # Loads the file into the recorder object property table, and starts
    # recording.

    public method record {} {
	global ::$w_.protect ::$w_.direction
	if {$recording_ == 1} {
	    return
	}
	if {[set $w_.protect]} {
	    error_dialog "Unset 'Protect' in order to write to file $imageFile_"
	    return
	}
	stop 0
	if {[check_file]} {
	    return
	}
	if {$camera_ == ""} {
	    error_dialog "You must choose a camera name"
	    return
	}	    
	$rtdplayback_ reset
	recorder_props
	if {[catch {$rtdrecorder_ record} msg]} {
	    error_dialog $msg
	    return
	}
	set recording_ 1
	set $w_.direction 0
	$itk_component(filename) config -value $imageFile_
	$itk_component(imagecount) config -value ""
	$itk_component(ncounts) config -value ""
	config -state disabled
	$itk_component(menubar).file config -state disabled
	$itk_component(menubar).options config -state disabled
	$itk_component(record) config -relief sunken
    }

    # start playback.

    public method play {} {
	global ::$w_.pbspeed
	if {[winfo exists $w_.subimage]} {
	    destroy $w_.subimage
	}
	set err 0
	if {[catch {set err [playcmd $itk_component(play) play]} msg] || $err} {
	    if {! $err} {
		warning_dialog "$msg"
	    }
	    return
	}
	set playing_ 1
	if {![$rtdplayback_ hastime] && [set $w_.pbspeed] == 2} {
	    warning_dialog "File does not include timestamp information for real-time playback"
	}
    }

    # check that the image file name is valid

    public method check_file {} {
	if {[llength $imageFile_] == 0} {
	    error_dialog "You must choose a file name"
	    return 1
	}
	return 0
    }

    
    # set the spool mode ("rewind", "ff").

    public method spool {arg} {
	set widg ""
	if {"$arg" == "ff"} {
	    set widg $itk_component(ff)
	}
	if {[playcmd $widg spool $arg] != 0} {
	    return
	}
	if {"$arg" == "ff"} {
	    set spooling_ 1
	}
    }

    # single step image

    public method single {} {
	playcmd "" step
    }

    # start playing back images

    public method playcmd {widg cmd args} {
	# Check the current state of the dialogue
	if {$recording_} {
	    return 1
	}
	set_normal
	if {[winfo exists $widg]} {
	    $widg config -relief sunken
	}
	stop 0
	if {[check_file]} {
	    return 1
	}

	# Attach the RTD camera to the recorder/playback tool.
	if {[attach_to_camera] != 0} {
	    return 1
	}

	# Reset the playback properties before starting
	if {[playback_props]} {
	    return 1
	}

	# Execute command
	set cmd "$rtdplayback_ $cmd [set args]"
	if {[catch {eval $cmd} msg]} {
	    stop
	    warning_dialog $msg
	    return 1
	}
	return 0
    }

    # stop the recorder

    public method stop {{hardstop 1}} {
	if {$hardstop} {
	    catch {$target_image_ detach_camera}
	    set attached_ 0
	}
	# Call the appropriate stop method
	if {$recording_ == 1} {
	    $rtdrecorder_ stop
	}
	if {$playing_ == 1 || $spooling_ == 1} {
	    $rtdplayback_ stop
	}
	# Configure the original server camera
	$mainwidg_ config -camera $itk_option(-server_camera)

	set recording_ 0
	set playing_ 0
	set spooling_ 0
	if {![winfo exists $w_]} {
	    return
	}
	if {$hardstop} {
	    set_normal
	}
    }

    
    # set the state to normal.

    public method set_normal {} {
	config -state normal
	$itk_component(menubar).file config -state normal
	$itk_component(menubar).options config -state normal
	foreach el {play ff rewind single record} {
	    $itk_component($el) config -relief raised
	}
    }

    # stop all actions and delete window.

    public method close { } {
	stop
	destroy $w_
    }

    # This method attaches the RTD to the RTDRPTOOL camera for the purposes
    # of image playback. A check is made to see if the RTD is already
    # attached.

    public method attach_to_camera {} {
	# If we are already attached then return immediately.
	if {$attached_ == 1} {
	    return 0
	}

	# Attach the RTD to RTDRPTOOL.
	if {[catch {$target_image_ attach_camera "RTDRPTOOL"} msg]} {
	    if {"$smg" != ""} {
		#warning_dialog $msg
	    }
	    return 1
	}
	set attached_ 1
        # Allow the re-attachment of the RTD to RTDRPTOOL to complete before 
        # the next action takes place; occasionally an image would be sent to
        # the rtdServer between detachment and re-attachment and so would be lost.
	update idletasks
	return 0
    }

    # go to the selected image

    public method gotoImage {arg} {
	global ::$w_.cmode ::$w_.direction
	if {$arg == ""} {
	    return
	}
	set cmodesv $w_.cmode
	if {![set $w_.cmode]} {
	    set $w_.cmode 1
	}
	if {[set $w_.direction]} {
	    incr arg 
	}
	playcmd "" gotoimage $arg
	single
	if {$cmodesv != [set $w_.cmode]} {
	    set $w_.cmode $cmodesv
	    playback_props
	}
    }

    # set global parameters

    public method set_globals {vars value} {
	foreach el [set vars] {
	    global ::$w_.$el
	    set $w_.$el $value
	}
    }

    #--------------------------------------------------------------------
    #  variables 
    #--------------------------------------------------------------------
    
    # target (main) RtdImage itcl widget
    itk_option define -target_image target_image Target_image {} {
	set target_image_ $itk_option(-target_image)
	set mainwidg_ [winfo parent [$target_image_ component hull]]
    }

    # font used for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*

    # font used for values
    itk_option define -valuefont valueFont ValueFont -Adobe-helvetica-medium-r-normal-*-12*

    # server camera name
    itk_option define -server_camera server_camera Server_camera {}

    # set the state to normal/disabled 
    itk_option define -state state State {normal}

    # main widget name
    protected variable mainwidg_
    
    # name of target rtd image
    protected variable target_image_

    # name of rtdrecorder image object for recording images
    protected variable rtdrecorder_

    # name of rtdplayback image object for playback of images
    protected variable rtdplayback_

    # flag: true if playing back images
    protected variable playing_ 0

    # flag: true if recording images
    protected variable recording_ 0
    
    # flag: true if spooling (rewind, ff)
    protected variable spooling_ 0

    # max record file size in MB
    protected variable maxFile_ 5
    
    # name of rtdimage camera for receiving images from the rtdserver
    protected variable camera_
    
    # file for recording images
    protected variable imageFile_ "RTDRPTOOL.fits"

    # Variables pertaining to the subimage sampling facility
    protected variable subimage_ 0

    protected variable x0_ 0
    protected variable y0_ 0
    protected variable width_ 0
    protected variable height_ 0

    # Variable for indicating whether RTD is currently attached to RTDRPTOOL.
    protected variable attached_ 0
}

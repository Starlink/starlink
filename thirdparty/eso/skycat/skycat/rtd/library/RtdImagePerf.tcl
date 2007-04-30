# E.S.O. - VLT project 
#
# RtdImagePerf.tcl - itcl widget to show current performance statistics.
#
# See man page RtdImagePerf(n) for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# D. Hopkinson    31 Jan 97  Created
# P.Biereichel    21/07/97   % display is the default + some bug fixes
# P.Biereichel    01/03/01   Only % display suported + code revised

itk::usual RtdImagePerf {}

# RtdImagePerf is an itcl widget to show current performance statistics.

itcl::class rtd::RtdImagePerf {
    inherit util::TopLevelWidget

    constructor {args} {
	eval itk_initialize $args
	wm title $w_ "Performance Test ($itk_option(-number))"
	wm protocol $w_ WM_DELETE_WINDOW "[code $this cancel]"
	make_layout
    }
    
    # make the window layout

    protected method make_layout {} {
	# main controls frame
	pack [set topf [frame $w_.topf]] -fill both -expand 1

	# frame for labels
	pack [set labelf [frame $topf.labelf -borderwidth 2 -relief groove]] \
		-fill x -expand 0

	make_labels $labelf
	add_buttons
    }
    
    protected method new_field {w label txtvar txtvar2 shelp} {
	set var $target_image_
	global ::$var

	util::LabelValue2 $w \
		-text $label \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief groove \
		-justify right \
		-orient horizontal \
		-shelp $shelp \
		-anchor w

	if {[lempty $label]} {
	    $w config -value $txtvar -value2 $txtvar2 -relief flat
	} else {
	    $w config -textvariable ${var}($txtvar) 
	    $w config -textvariable2 ${var}($txtvar2)
	}
    }
    
    # make the window to display the statistics in the given frame
    
    protected method make_labels {w} {
	set var $target_image_
	global ::$var

	new_field $w.actual "" "Actual" "Average" ""
	new_field $w.freq   "Update Frequency:(Hz)" PERF_FREQ PERF_FREQ_AVE \
		"Image Update Frequency (Hz)"
	new_field $w.gen    "Processing time:\t(%)" PERF_GEN PERF_GEN_AVE \
		"General Code Processing"
	new_field $w.tclf    "Tcl code:\t\t(%)" PERF_TCL PERF_TCL_AVE \
		"TCL code interpretation"
	new_field $w.xf    "X Function calls:\t(%)" PERF_XFUNC PERF_XFUNC_AVE \
		"X function calls"
	new_field $w.tf    "Total time/image\t(msec):" PERF_TOTAL PERF_TOTAL_AVE \
		"Total time spent per image event (msec)"

	itk_component add count {
	    LabelValue $w.count \
		    -textvariable ${var}(PERF_COUNT) \
		    -text "Image counter:\t\t" \
		    -labelfont $itk_option(-labelfont) \
		    -valuefont $itk_option(-valuefont) \
		    -valuewidth $itk_option(-valuewidth) \
		    -labelwidth $itk_option(-labelwidth) \
		    -relief groove \
		    -justify right \
		    -anchor w
	}
	pack $itk_component(count) \
		-side bottom -padx 1m -pady 0m -anchor w -fill none -expand 1
	[$itk_component(count) component entry] config -justify right
	add_short_help $itk_component(count) "Number of image events:"
    }
    
    
    # add a row of dialog buttons at the bottom of the window
    
    protected method add_buttons {} {
	
	# dialog buttons frame
	itk_component add buttons {
	    frame $w_.buttons -borderwidth 2 -relief groove
	}
	pack $itk_component(buttons) \
		-side top -fill x -expand 1 -padx 1m -pady 1m
	foreach {btn text shelp} { \
		save   "Save..."  "Save: save the current performance settings to file" \
		attach "Attach"   "Attach: attach to camera" \
		reset  "Reset"    "Reset: start new averaging" \
		cancel "Close"    "Close: close the test window"} {
	    itk_component add $btn {
		button $w_.$btn \
			-text $text \
			-command [code $this $btn]
	    }
	    add_short_help $itk_component($btn) $shelp
	    pack $itk_component($btn) \
		    -side left -expand 1 -padx 2m -pady 2m -in $itk_component(buttons)
	}
    }

    # attach to camera

    public method attach {} {
	global env
	if {! [catch {$itk_option(-target_image) attach_camera $env(RTD_CAMERA)} msg]} {
	    return 
	}
	error_dialog $msg
    }

    # Save the current performance parameters and image information to file.
    public method save {} {
	set var $target_image_
	global ::$var

	set genvalue [set ${var}(PERF_GEN)]
	set tclvalue [set ${var}(PERF_TCL)]
	set xvalue [set ${var}(PERF_XFUNC)]
	set timevalue [set ${var}(PERF_TOTAL)]

	# Check that there is something to save before doing anything else.
	if {$genvalue == "" || $tclvalue == "" || $xvalue == ""} {
	    return
	}

	# Get the required filename to save to.
	set imageFile [filename_dialog [pwd] * $w_]
	if {$imageFile == ""} {
	    return
	}

	set file [open $imageFile w]

	puts $file "Performance Statistics Output"
	puts $file ""
	puts $file "STATS: Units - percentage of total time/image event"
	puts $file "General processing: $genvalue"
	puts $file "TCL code interpretation: $tclvalue"
	puts $file "X function calls: $xvalue"
	puts $file "Total time/image event (msecs): $timevalue"
	puts $file ""

	puts $file "IMAGE INFORMATION:"
	puts $file "Image width: [$target_image_ width]"
	puts $file "Image height: [$target_image_ height]"
	puts $file "Bytes/pixel: [expr {abs([$target_image_ bitpix])}]"
	puts $file "Image size (bytes): [expr {[$target_image_ width] * \
		[$target_image_ height] * abs([$target_image_ bitpix]) / 8}]"
	puts $file ""

	puts $file "DISPLAY INFORMATION:"
	puts $file "Unscaled height: [$target_image_ height]"
	puts $file "Unscaled width: [$target_image_ width]"
	puts $file "Scaled height: [$target_image_ dispheight]"
	puts $file "Scaled width: [$target_image_ dispwidth]"
	puts $file "Scaling (x y): [$target_image_ scale]"
	puts $file "Flip (X): [$target_image_ flip x]"
	puts $file "Flip (Y): [$target_image_ flip y]"
	puts $file "Rotation: [$target_image_ rotate]"

	close $file
    }
    
    public method reset {} {
	$target_image_ perftest reset
    }

    # close this window
    public method cancel {} {
	$target_image_ perftest off
	destroy $w_
    }

    # -- options --

    # target (main) RtdImage itcl widget
    itk_option define -target_image target_image Target_image {} {
	set target_image_ [$itk_option(-target_image) get_image]
    }

    # font used for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*

    # font used for values
    itk_option define -valuefont valueFont ValueFont -Adobe-helvetica-medium-r-normal-*-12*

    # set the width for  displaying labels
    itk_option define -labelwidth labelWidth LabelWidth 21

    # set the width for  displaying values
    itk_option define -valuewidth valueWidth ValueWidth 8

    # -- protected vars -- 

    # internal target image
    protected variable target_image_
}

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

itk::usual RtdImagePerf {}

# RtdImagePerf is an itcl widget to show current performance statistics.

itcl::class rtd::RtdImagePerf {
    inherit util::TopLevelWidget

    #  create a new RtdImagePerf widget

    constructor {args} {
	# do this first so we can use the option values
	eval itk_initialize $args
	wm title $w_ "Interactive Performance Test ($itk_option(-number))"
	wm protocol $w_ WM_DELETE_WINDOW ""
	make_layout
	set_units $current_display_mode
    }

    
    # destructor

    destructor {
	catch {$target_image_ perftest off}
	catch {$target_image_ view remove $image_}
    }

    
    # do the window layout

    protected method make_layout {} {
	# main controls frame
	pack [set mainf [frame $w_.mainf]] \
	    -side top -fill both -expand 1 -padx 1m -pady 1m -ipadx 1m -ipady 1m

	pack [set topf [frame $mainf.topf]] \
	    -side top -fill both -expand 1

	make_labels $topf
	add_buttons
    }


    # make the window to display the statistics in the given frame

    protected method make_labels {w} {
	set var $target_image_
	global ::$var

	# frame for labels
	pack [set labelf [frame $w.labelf -borderwidth 2 -relief groove]] \
	    -fill both -expand 0

	# frame for units label menu
	pack [set unitsf [frame $w.unitsf -borderwidth 2 -relief groove]] \
	    -fill both -expand 0 -pady 2m
	
	set lablf [frame $labelf.labl -borderwidth 2]
	set freqf [frame $labelf.freqf -borderwidth 2]
	set genf [frame $labelf.genf -borderwidth 2]
	set tclf [frame $labelf.tclf -borderwidth 2]
	set memf [frame $labelf.memf -borderwidth 2]
	set xf [frame $labelf.xf -borderwidth 2]
	set totalf [frame $labelf.totalf -borderwidth 2]

	# LabelValue(n) widget "Last image"
	itk_component add labl {
	    LabelValue $lablf.labl \
		-text "                     " \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-labelfont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief flat \
		-anchor e \
		-value "Last image"
	}
	# LabelValue(n) widget "Average"
	itk_component add labl_ave {
	    LabelValue $lablf.labl_ave \
		-text "" \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-labelfont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief flat \
		-anchor e \
		-value "Average"
	}
	pack $itk_component(labl) $itk_component(labl_ave) \
	    -side left -padx 1m -pady 0m

	# LabelValue(n) widget "Update Frequency (Hz)"
	itk_component add freq {
	    LabelValue $freqf.freq \
		-text "Update Frequency (Hz):" \
		-textvariable ${var}(FREQ) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief groove \
		-anchor e
	}
	# LabelValue(n) widget for average frequency
	itk_component add freq_ave {
	    LabelValue $freqf.freq_ave \
		-text "" \
	        -textvariable ${var}(FREQ_AVE) \
		-valuefont $itk_option(-valuefont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth 0 \
		-relief groove \
		-anchor e
	}
	pack $itk_component(freq) $itk_component(freq_ave) \
	    -side left -padx 1m -pady 0m

	# LabelValue(n) widget: "General Code Processing (s)"
	itk_component add gen {
	    LabelValue $genf.gen \
		-text "General Code Processing (s):" \
		-textvariable ${var}(GEN) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth $itk_option(-labelwidth) \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	# LabelValue(n) widget for average general image processing
	itk_component add gen_ave {
	    LabelValue $genf.gen_ave \
		-text "" \
		-textvariable ${var}(GEN_AVE) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth 0 \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	pack $itk_component(gen) $itk_component(gen_ave) \
	    -side left -padx 1m -pady 0m

	# LabelValue(n), "Memory Management (s)"
	itk_component add mem {
	    LabelValue $memf.mem \
		-text "Memory Management (s):" \
		-textvariable ${var}(MEM) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth $itk_option(-labelwidth) \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	# LabelValue(n) widget for average memory management
	itk_component add mem_ave {
	    LabelValue $memf.mem_ave \
		-text "" \
		-textvariable ${var}(MEM_AVE) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth 0 \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	pack $itk_component(mem) $itk_component(mem_ave) \
	    -side left -padx 1m -pady 0m
 
	# LabelValue(n) widget: "TCL Code Interpretation"
	itk_component add tcl {
	    LabelValue $tclf.tcl \
		-text "TCL Code Interpretation (s):" \
		-textvariable ${var}(TCL) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth $itk_option(-labelwidth) \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	# LabelValue(n) widget: average TCL code interpretation
	itk_component add tcl_ave {
	    LabelValue $tclf.tcl_ave \
		-text "" \
		-textvariable ${var}(TCL_AVE) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth 0 \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	pack $itk_component(tcl) $itk_component(tcl_ave) \
	    -side left -padx 1m -pady 1m
 
	# LabelValue(n) widget: "X Function Calls"
	itk_component add x {
	    LabelValue $xf.x \
		-text "X Function Calls (s):" \
		-textvariable ${var}(XFUNC) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth $itk_option(-labelwidth) \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	# LabelValue(n) widget: average X function calls
	itk_component add x_ave {
	    LabelValue $xf.x_ave \
		-text "" \
		-textvariable ${var}(XFUNC_AVE) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth 0 \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	pack $itk_component(x) $itk_component(x_ave) \
	    -side left -padx 1m -pady 0m

	# LabelValue(n) widget: "Total time/image"
	itk_component add total {
	    LabelValue $totalf.total \
		-text "Total time/image (s):" \
		-textvariable ${var}(TOTAL) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth $itk_option(-labelwidth) \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	# LabelValue(n) widget: average total time spent per image event
	itk_component add total_ave {
	    LabelValue $totalf.total_ave \
		-text "" \
		-textvariable ${var}(TOTAL_AVE) \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth 0 \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	}
	pack $itk_component(total) $itk_component(total_ave) \
	    -side left -padx 1m -pady 0m

	# LabelMenu widget: "Select display mode:"
	itk_component add set_units {
	    LabelMenu $unitsf.units \
		-text "Select display mode:" \
		-relief raised \
		-orient horizontal
	}
	$itk_component(set_units) add -label "Percentage time" \
	    -command [code $this set_units 2]
	$itk_component(set_units) add -label "Normalised time" \
	    -command [code $this set_units 1]
	$itk_component(set_units) add -label "Time/image event" \
	    -command [code $this set_units 0]

	# XXXX This component is presently unused - could be reinstated later.
	
	# LabelEntry(n) widget "Max frq:"
	itk_component add maxfreq {
	    LabelEntry $labelf.maxfreq \
		-text "Max frq:" \
		-command [code $this set_required_frequency] \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief sunken \
		-anchor e
	}

	pack \
	    $lablf $freqf $genf $tclf $memf $xf $totalf \
	    -side top
	pack $itk_component(set_units) -side top -padx 2m -pady 2m

	add_short_help $itk_component(freq) {Image Update Frequency (Hz)}
	add_short_help $itk_component(freq_ave) {Average Image Update Frequency}
	add_short_help $itk_component(gen) {General image processing code}
	add_short_help $itk_component(gen_ave) {Average General image processing code}
	add_short_help $itk_component(mem) {Memory management code}
	add_short_help $itk_component(mem_ave) {Average Memory management code}
	add_short_help $itk_component(tcl)  {TCL code interpretation}
	add_short_help $itk_component(tcl_ave)  {Average TCL code interpretation}
	add_short_help $itk_component(x)    {X function calls}
	add_short_help $itk_component(x_ave)    {Average X function calls}
	add_short_help $itk_component(total) {Total time spent per image event (seconds)}
	add_short_help $itk_component(total_ave) {Average Total time spent per image event (seconds)}
	add_short_help $itk_component(set_units) {Set the mode for the performance parameters display}
    }

    # add a row of dialog buttons at the bottom of the window

    protected method add_buttons {} {

	# dialog buttons frame
	itk_component add buttons {
	    frame $w_.buttons -borderwidth 2 -relief groove
	}
	pack $itk_component(buttons) \
	    -side top -fill x -padx 1m -pady 1m

	# Save button
	itk_component add save {
	    button $w_.save \
		-text "Save" \
		-command [code $this save]
	}

	# Reset button
	itk_component add reset {
	    button $w_.reset \
		-text "Reset" \
		-command [code $this reset]
	}

	# Close button
	itk_component add close {
	    button $w_.close \
		 -text "Close" \
		 -command [code $this cancel]
	}

	pack \
	    $itk_component(save) $itk_component(reset) $itk_component(close) \
	    -side left -expand 1 -padx 2m -pady 2m -in $itk_component(buttons)
	add_short_help $itk_component(save) {Save: save the current performance settings to file}
	add_short_help $itk_component(close) {Close: close the test window}
	add_short_help $itk_component(reset) {Reset: start new averaging}
    }

    # Set the required units that the performance parameters are to be
    # displayed in.
    public method set_units {unit_type} {

	convert_units $unit_type

	# Change the labels according to the units
	if {$unit_type == 1} {
	    # Normalised...
	    $itk_component(gen) configure -text "General Code Processing (s):"
	    $itk_component(mem) configure -text "Memory Management (norm):"
	    $itk_component(x) configure -text "X Function Calls (norm):"
	    $itk_component(tcl) configure  -text "TCL Code Interpretation (s):"
	} elseif {$unit_type == 2} {
	    # Percentage time
	    $itk_component(gen) configure -text "General Code Processing (%):"
	    $itk_component(mem) configure -text "Memory Management (%):"
	    $itk_component(x) configure -text "X Function Calls (%):"
	    $itk_component(tcl) configure  -text "TCL Code Interpretation (%):"
	} elseif {$unit_type == 0} {
	    # Seconds/image event
	    $itk_component(gen) configure -text "General Code Processing (s):"
	    $itk_component(mem) configure -text "Memory Management (s):"
	    $itk_component(x) configure -text "X Function Calls (s):"
	    $itk_component(tcl) configure  -text "TCL Code Interpretation (s):"
	}

	# Change the units for future image updates
	$target_image_ perftest convert $unit_type
	set current_display_mode $unit_type
    }

    public method reset {} {
	$target_image_ perftest reset
    }

    # This method takes the performance parameters from the dialogue and
    # converts them to the new display mode, before placing them back in the
    # dialogue.
    public method convert_units {new_units} {
	set var $target_image_
	global ::$var

	# Get the current settings.
	set genvalue [set ${var}(GEN)]
	set memvalue [set ${var}(MEM)]
	set tclvalue [set ${var}(TCL)]
	set xvalue [set ${var}(XFUNC)]
	set timevalue [set ${var}(TOTAL)]

	if {$genvalue == "" || $memvalue == "" || $tclvalue == "" || \
	    $xvalue == ""} {
	    return
	}

	set scale [expr [$target_image_ width] * \
		[$target_image_ height] * [$target_image_ bitpix] / 1024]
	set scale [expr abs($scale) / 8]

	# First put everything back in terms of absolute times.
	if {$current_display_mode == 1} {
	    # current units are normalised.
	    set xvalue [expr $xvalue * $scale]
	    set memvalue [expr $memvalue * $scale]
	} elseif {$current_display_mode == 2} {
	    # current units are percentages
	    set xvalue [expr $timevalue * [expr $xvalue / 100]]
	    set memvalue [expr $timevalue * [expr $memvalue / 100]]
	    set genvalue [expr $timevalue * [expr $genvalue / 100]]
	    set tclvalue [expr $timevalue * [expr $tclvalue / 100]]
	}

	# Now put everything into the required units.
	if {$new_units == 1} {
	    set xvalue [expr $xvalue / $scale]
	    set memvalue [expr $memvalue / $scale]
	} elseif {$new_units == 2} {
	    set xvalue [expr 100 * [expr $xvalue / $timevalue]]
	    set memvalue [expr 100 * [expr $memvalue / $timevalue]]
	    set genvalue [expr 100 * [expr $genvalue / $timevalue]]
	    set tclvalue [expr 100 * [expr $tclvalue / $timevalue]]
	}

	# Finally put everything back into the dialogue.
	set memvalue [format "%9.6f" $memvalue]
	set xvalue [format "%9.6f" $xvalue]
	set genvalue [format "%9.6f" $genvalue]
	set tclvalue [format "%9.6f" $tclvalue]
	$itk_component(mem) configure -value $memvalue
	$itk_component(x) configure -value $xvalue
	$itk_component(gen) configure -value $genvalue
	$itk_component(tcl) configure -value $tclvalue
    }

    # Save the current performance parameters and image information to file.
    public method save {} {
	set var $target_image_
	global ::$var

	set genvalue [set ${var}(GEN)]
	set memvalue [set ${var}(MEM)]
	set tclvalue [set ${var}(TCL)]
	set xvalue [set ${var}(XFUNC)]
	set timevalue [set ${var}(TOTAL)]

	# Check that there is something to save before doing anything else.
	if {$genvalue == "" || $memvalue == "" || $tclvalue == "" || \
	    $xvalue == ""} {
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
	if {$current_display_mode == 0} {
	    puts $file "STATS: Units - seconds/image event"
	} elseif {$current_display_mode == 1} {
	    puts $file "STATS: Units - seconds/image event, normalised for\
		memory management and X calls per Kb of image"
	} else {
	    puts $file "STATS: Units - percentage of total time/image event"
	}
	puts $file "Memory management: $memvalue"
	puts $file "General processing: $genvalue"
	puts $file "TCL code interpretation: $tclvalue"
	puts $file "X function calls: $xvalue"
	puts $file "Total time/image event (secs): $timevalue"
	puts $file ""

	puts $file "IMAGE INFORMATION:"
	puts $file "Image width: [$target_image_ width]"
	puts $file "Image height: [$target_image_ height]"
	puts $file "Bytes/pixel: [expr abs([$target_image_ bitpix])]"
	puts $file "Image size (bytes): [expr [$target_image_ width] * \
	    [$target_image_ height] * [expr abs([$target_image_ bitpix]) / 8]]"
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
    itk_option define -labelwidth labelWidth LabelWidth 25

    # set the width for  displaying values
    itk_option define -valuewidth valueWidth ValueWidth 13

    # -- protected vars -- 
    
    # internal target image
    protected variable target_image_

    # internal target canvas
    protected variable target_canvas_
    
    # internal canvas widget
    protected variable canvas_

    # internal rtd image
    protected variable image_

    # Current unit type
    protected variable current_display_mode 2
}

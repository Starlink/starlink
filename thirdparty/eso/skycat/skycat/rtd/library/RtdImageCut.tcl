#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImageCut.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# RtdImageCut.tcl - itcl widget for setting cut levels for an RtdImage widget
# 
# See man page RtdImageCut(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01/06/95   Created
#
# P. Biereichel   09/10/97   Enter cut sets also scale range
#
# Allan Brighton  24/03/98   Added $initialized_ variable to fix bug
#                            when image is blank (generated for plotting).
#                            Added Peter Draper's (Starlink) fixes, 
#                            bindings, -resolution argument.
#
# Allan Brighton  10/04/98   Moved constructor code to "init" method for
#                            easier subclassing.
#                            Removed bg_set and the code for setting the
#                            color on the "Set" button, as it didn't always 
#                            work correctly.
#                            Removed the calls to "update_increment", since it
#                            caused endless event looping in some cases.
#                            (The event code is getting too complex to follow...)

itk::usual RtdImageCut {}

# This widget displays a toplevel window containing a plot of the pixel
# value distribution in the target image and buttons and scales for
# manipulating the image cut levels. The cut levels are two values: the
# lowest and highest pixel values considered when color scaling the
# image, i.e.: mapping image pixel values to color values, and are used
# to filter out noise and other extreme values in the image.
#
# The plot displayed uses the rtdimage "getdist" subcommand to get the
# pixel value distribution. This is an array of values that specify, for
# example, how many pixel values are between 0 and 100, between 100 and
# 200, and so on. This information can also be used to set the cut levels,
# by specifying a percent of the total number of pixels that should be within
# the cut levels.
#
# A second, faster algorithm is supported by the "Median Filter" button.
# This is a standard algorithm that works very fast to determine
# reasonable cut levels.
#
# After setting the cut levels, either manually or by one of the
# buttons, the new pixel value distribution is displayed. The plotting
# is done directly from the rtdimage C++ code to the BLT graph over its
# C interface.

itcl::class rtd::RtdImageCut {
    inherit util::TopLevelWidget


    #  constructor: create a new instance of this class

    constructor {args} {
	eval itk_initialize $args
    }


    # destructor - clean up when deleted
    
    destructor {
	global ::tcl_version
	if {$tcl_version >= 8.0} {
	    blt::vector destroy $xVector_ $yVector_
	}
    }

    # called after constructors have run

    method init {} {
	wm minsize $w_ 10 10
	wm title $w_ "Cut Levels ($itk_option(-number))"
	wm iconname $w_ "Cut Levels ($itk_option(-number))"

	# get internal image handle
	set image_ [$itk_option(-image) get_image]

	make_graph
	make_controls
	make_buttons
	make_short_help
	incr initialized_
	update_graph 0
    }
    
    
    # add a short help window

    protected method make_short_help {} {
	# TopLevelWidget::make_short_help
	add_short_help $itk_component(graph) \
	    {Graph: shows distribution of pixel values in image \
		 {bitmap b1} ... {bitmap b1} = Zoom in, {bitmap b3} = Zoom out, Cancel zoom}

	add_short_help $itk_component(percent) \
	    {Auto set: {bitmap b1} = set so that given percent of pixels are within cut levels}

	add_short_help [$itk_component(lowcut) component scale] \
	    {Low cut: {bitmap dragb1} = adjust low cut value}

	add_short_help [$itk_component(lowcut) component entry] \
	    {Low cut: enter low cut value and set low cut scale range}

	add_short_help [$itk_component(highcut) component scale] \
	    {High cut: {bitmap dragb1} = adjust high cut value}

	add_short_help [$itk_component(highcut) component entry] \
	    {High cut: enter high cut value and set high cut scale range}

	add_short_help $itk_component(set) \
	    {Set: {bitmap b1} = set cut levels to selected values}

	add_short_help $itk_component(reset) \
	    {Reset: {bitmap b1} = apply min, max to set cut levels}

	add_short_help $itk_component(median) \
	    {Auto Set Cut Levels: {bitmap b1} = apply median filtering algorithm to set cut levels}

	add_short_help $itk_component(update) \
	    {Update: {bitmap b1} = update pixel value distribution (e.g. after a real-time image event)}

	add_short_help $itk_component(close) \
	    {Close: {bitmap b1} = close this window}
    }
    
    
    # make the graph subwindow

    protected method make_graph {} {
	global ::tcl_version
	# BLT graph widget for displaying pixel distribution
	itk_component add graph {
	    blt::graph $w_.graph \
		-width 4i \
		-height 3i \
		-borderwidth 2 \
		-relief groove \
		-title "Pixel Value Distribution"
	} {
	}
	set graph_ $w_.graph
	pack $itk_component(graph) \
	    -fill both -expand 1 -padx 1m -pady 1m
	
	$graph_ xaxis configure -title {} -command [code $this notify_blt_zoom]
	$graph_ yaxis configure -title {}

	# tcl8/blt2.4f vector names ust start with a letter, no dots...
	# Someone also changed the default symbol to circle also - why?
	regsub -all {\.} v$graph_.xVector _ xVector_ 
	regsub -all {\.} v$graph_.yVector _ yVector_ 
	if {$tcl_version >= 8.0} {
	    $graph_ legend config -hide 1
	    if {![info exists $xVector_]} {
		blt::vector create $xVector_ $yVector_
	    }
	    set symbol {}
	} else {
	    global ::$xVector_ ::$yVector_
	    $graph_ legend config -mapped 0
	    if {![info exists $xVector_]} {
		blt::vector $xVector_ $yVector_
	    }
	    set symbol none
	}
	
	$graph_ element create elem -xdata $xVector_ -ydata $yVector_ -symbol $symbol
	# plot the distribution of pixel values
	if {[catch {$image_ graphdist $graph_ elem $itk_option(-num_points) \
			$xVector_ $yVector_} msg]} {
	    #warning_dialog $msg
	}

	# add BLT features
	::Blt_ZoomStack $graph_
	::Blt_ActiveLegend $graph_
	::Blt_ClosestPoint $graph_
    }

    # make the control panel

    protected method make_controls {} {
	# frame containing scale widgets to adjust cut levels
	itk_component add scales {
	    frame $w_.scales -borderwidth 2 -relief groove
	}
	pack $itk_component(scales) \
	    -side top -fill x -padx 1m -pady 1m

	# LabelChoice(n) widget displaying percent values for setting cut levels
	itk_component add percent {
	    util::LabelChoice $itk_component(scales).percent \
		-text "Auto Set:" \
		-choice "90% 95% 98% 99% 99.5% 100%" \
		-value "" \
		-command [code $this set_by_percent]
	}
	foreach lab "Low High" {
	    set el [string tolower $lab]
	    # Lowscale and Highscale component frames
	    itk_component add ${el}scale {
		frame $itk_component(scales).${el}scale -borderwidth 2 -relief groove
	    }
	    global ::$w_.${el}cut
	    # Lowcut and Highcut components (LabelEntryScale(n) widgets)
	    # for displaying and adjusting the cut levels
	    itk_component add ${el}cut {
		util::LabelEntryScale $itk_component(${el}scale).${el}cut \
		    -text $lab \
		    -valuewidth 8 \
		    -show_arrows 1 \
		    -validate real \
		    -value 0 \
		    -command [code $this setb_${el}cut 0] \
		    -entrycommand [code $this setb_${el}cut 1]
	    }
	    $itk_component(${el}cut) component scale config -variable $w_.${el}cut
	    
	    pack $itk_component(${el}cut) -padx 1m
	}

        blt::table $itk_component(scales) \
            $itk_component(lowscale)       1,0 -anchor w -fill x -padx 1m -pady 1m \
            $itk_component(highscale)      1,1 -anchor w -fill x -padx 1m -pady 1m \
            $itk_component(percent)        2,0 -anchor c -fill x -columnspan 2 -padx 1m \
    }

    
    # make the button frame at the bottom of the window

    protected method make_buttons {} {
	# Tk frame for buttons
	itk_component add buttons {
	    frame $w_.buttons -borderwidth 2 -relief groove
	}
	pack $itk_component(buttons) \
	    -side top -fill x -padx 1m -pady 1m

	add_button set Set set_cutlevels
	add_button reset Reset reset_cutlevels
	add_button median "Median Filter" set_by_median
	add_button update Update update_graph
	add_button close Close quit
	
        blt::table $itk_component(buttons) \
            $itk_component(set)       1,0 -anchor w -fill x -padx 1m -pady 2m \
            $itk_component(reset)     1,1 -anchor w -fill x -padx 1m -pady 2m \
            $itk_component(median)    1,2 -anchor w -fill x -padx 1m -pady 2m \
            $itk_component(update)    1,3 -anchor w -fill x -padx 1m -pady 2m \
            $itk_component(close)     1,4 -anchor w -fill x -padx 1m -pady 2m
    }

    # add a button to the buttons frame

    public method add_button {compo text command} {
	# Button components: set, reset, median, update, or close
	itk_component add $compo {
	    button $itk_component(buttons).$compo \
		-text $text \
		-command [code $this $command]
	}
    }
    
    # update the graph after a new image has been loaded or the image has been
    # modified

    public method update_graph {{modimg 1}} {
	# no image is loaded ? 
	if {[$image_ isclear] || ! $initialized_} {
	    quit
	    return
	}
	if {$modimg} {
	    lassign [$image_ cut] low high
	} else {
	    set low_ [set low [$image_ min]]
	    entry_value lowcut $low
	    set high_ [set high [$image_ max]]
	    entry_value highcut $high
	}
	if {$low > $high} {
	    lassign "$low $high" high low
	}
	lassign [get_cuts] plow phigh
	if {$plow != $low || $phigh != $high} {
	    $itk_component(percent) config -value ""
	}

	# adapt new scale range
	setb_lowcut [expr {$low <= $low_}] $low
	setb_highcut [expr {$high >= $high_}] $high
	update

	# plot the distribution of pixel values
	if {[catch {
	    $image_ graphdist $graph_ elem $itk_option(-num_points) \
		$xVector_ $yVector_} msg]} {
	    #warning_message $msg
	}
	update_increment
    }

    # return the current low/high cut values

    public method get_cuts {} {
	set low [$itk_component(lowcut) get]
	set high [$itk_component(highcut) get]
	if {"$low" == ""} {
	    set low [$image_ min]
	}
	if {"$high" == ""} {
	    set high [$image_ max]
	}
	return "$low $high"
    }

    # this method is called when blt changes the tick labels after a zoom

    protected method notify_blt_zoom {pathname tickvalue} {
	if {[$image_ isclear] || ! $initialized_} {
	    quit
	    return
	}
	set min [$graph_ xaxis cget -min]
	set max [$graph_ xaxis cget -max]
	lassign [get_cuts] low high
	if {$min != $low || $max != $high} {
	    setb_cut 0 $min $max
	}
	return $tickvalue
    }

    # update xaxis after rescaling

    protected method update_xaxis {{value 0}} {
	lassign [get_cuts] low high
	if {$low >= $high} {
	    return
	}
	set clow [$graph_ xaxis cget -min]
	set chigh [$graph_ xaxis cget -max]
	if {$low != $clow || $high != $chigh} {
	    $graph_ xaxis configure -min $low -max $high
	}
    }

    # set low and high cut

    protected method setb_cut {flg low high} {
	setb_lowcut $flg $low
	setb_highcut $flg $high
    }

    # write value into entry field

    protected method entry_value {compo value} {
	set w [$itk_component($compo) component entry]
        $w delete 0 end
        $w insert 0 $value
    }

    
    # update the increment for the slider buttons depending on the range
    # XXX not currently used - problems with looping in event handlers (allan)

    protected method update_increment {} {
	lassign [get_cuts] low high
	set increment [expr {($high-$low)/100.0}]
	if {$increment <= 0} {
	    return
	}
	set resolution $increment
	$itk_component(lowcut) configure -increment $increment \
	    -resolution $resolution
	$itk_component(highcut) configure -increment $increment \
	    -resolution $resolution
    }

    # set entry values of the lowcut scale widget and update scale widgets

    protected method setb_lowcut {setlow value} {
	lassign [get_cuts] low high
	if {$value >= $high} {
	    set value [expr {$high - 1}]
	}
	if {$setlow} {
	    set low_ $value
	}
	entry_value lowcut $value
	update_cut $value $high
    }

    # set entry values of the highcut scale widget and update scale widgets

    protected method setb_highcut {sethigh value} {
	lassign [get_cuts] low high
	
	if {$value <= $low} {
	    set value [expr {$low + 1}]
	}
	if {$sethigh} {
	    set high_ $value
	}
	entry_value highcut $value
	update_cut $low $value
    }

    # update min, max values of the lowcut and highcut scale widgets

    protected method update_cut {low high} {
	foreach el "lowcut highcut" {
	    update_$el $low $high
	}
	update_xaxis
    }

    # update min, max values of the lowcut scale widget

    protected method update_lowcut {low high} {
	global ::$w_.lowcut
	set from $low_
	set to [expr {$high - 1.0}]
	if {$from > $low} {
	    set from $low
	}
	if {$to < $low} {
	    set to $low
	}
	$itk_component(lowcut) config -from $from -to $to
	set $w_.lowcut $low
    }

    # update min, max values of the highcut scale widget

    protected method update_highcut {low high} {
	global ::$w_.highcut
	set from [expr {$low + 1.0}]
	set to $high_
	if {$from > $high} {
	    set from $high
	}
	if {$to < $high} {
	    set to $high
	}
	if {$from == $to} {
	    set from [expr {$from - 1.0}]
	}
	$itk_component(highcut) config -from $from -to $to
	set $w_.highcut $high
    }

    # set the cut levels in the image

    protected method set_cutlevels {} {
	lassign [get_cuts] low high
	busy {$image_ cut $low $high}
	if {"$itk_option(-command)" != ""} {
	    eval $itk_option(-command)
	}
	$itk_component(percent) config -value ""
    }

    # reset the cut levels to the original min/max values

    public method reset_cutlevels {} {
	set low [$image_ min]
	set high [$image_ max]
	busy {$image_ cut $low $high}
	setb_cut 1 $low $high
	$itk_component(percent) config -value 100
	if {"$itk_option(-command)" != ""} {
	    eval $itk_option(-command)
	}
    }


    # automatically set the cut values by percent of distribution
    # that should be inside the cut levels

    public method set_by_percent {percent} {
	scan $percent "%d" percent
	busy {$image_ autocut -percent $percent}
	lassign [$image_ cut] low high
	setb_cut 1 $low $high
	if {"$itk_option(-command)" != ""} {
	    eval $itk_option(-command)
	}
    }


    # automatically set the cut values by median filtering

    public method set_by_median {} {
	busy {$image_ autocut}
	lassign [$image_ cut] low high
	setb_cut 1 $low $high
	if {"$itk_option(-command)" != ""} {
	    eval $itk_option(-command)
	}
	$itk_component(percent) config -value ""
    }


    # -- public vars --

    # target RtdImage itcl class object
    itk_option define -image image Image {}

    # number of points to plot
    itk_option define -num_points num_points Num_points {2048}

    # tcl command to evaluate when cut levels are changed
    itk_option define -command command Command {}



    # -- protected vars --
    
    # internal rtdimage object
    protected variable image_

    # name of graph widget
    protected variable graph_

    # lowest value to display (image(min) or set by user)
    protected variable low_ 0

    # highest value to display (image(max) or set by user)
    protected variable high_ 1

    # set to 1 after widget has been initizlized
    protected variable initialized_ 0

    # x vector for graph
    protected variable xVector_ 
   
    # y vector for graph
    protected variable yVector_ 
}

#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImageSpectrum.tcl,v 1.22 1998/11/16 21:24:33 abrighto Exp $"
#
# RtdImageSpectrum.tcl - itcl widget for displaying graph of image data values 
#                        along a line
# 
# See man page RtdImageSpectrum(n) for a complete description.
#
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
# Allan Brighton  28 Jun 96  Changed GUI name to "Cuts", 
#                            (suggested by M. Albrecht)
# Peter Bieeichel 22/07/97   Added value display at cursor position

itk::usual RtdImageSpectrum {}

# This [incr Tk] widget is used to display a BLT graph in a
# popup window plotting the raw image pixel values along a given
# line drawn interactively on the image. Once created, the graph
# can be continuously updated as the line is moved or resized.
# This widget only sets up the layout. The real work is done in
# the rtdimage spectrum subcommand (see rtdimage(3)), that communicates
# directly with the BLT graph using its C interface.

itcl::class rtd::RtdImageSpectrum {
    inherit util::TopLevelWidget


    #  constructor: create a new instance of this class

    constructor {args} {
	eval itk_initialize $args

	wm minsize $w_ 10 10
	wm title $w_ "Cuts ($itk_option(-number))"

	make_graph
	make_buttons
	$draw_ add_notify_cmd $itk_option(-line_id) [code $this notify_cmd] 1
    }
    
 
    # destructor - clean up when deleted
    
    destructor {
	global ::tcl_version
	$draw_ remove_notify_cmd $itk_option(-line_id)
	$draw_ delete_object $itk_option(-line_id)
 	if {$tcl_version >= 8.0} {
	    blt::vector destroy $xVector_ $yVector_
	}
   }

   
    # make the graph subwindow

    protected method make_graph {} {
	global ::tcl_version
	# Note: make the graph in the global namespace for now so that
	# the old blt utils (features.tcl) still work. Shouldn't be needed
	# with blt-2.0 or later...
        set cmd \
		[list blt::graph $w_.graph \
		-width 5i \
		-height 3i \
		-borderwidth 3 \
		-relief groove \
		-title "Pixel Values"]
	# BLT graph of pixel values
        itk_component add graph {
            set graph_ [uplevel "#0" $cmd]
        } {
        }
	pack $itk_component(graph) \
		-fill both -expand 1 -padx 1m -pady 1m

	add_short_help $itk_component(graph) \
	    {Graph: plot image pixel values along line, {bitmap dragb1} = zoom, {bitmap b2} = restore}

	$graph_ yaxis configure -title {}

	# blt2.4f vector names ust start with a letter, no dots...
	# someone also changed the default symbol to circle. Why?
	regsub -all {\.} v$graph_.xVector _ xVector_ 
	regsub -all {\.} v$graph_.yVector _ yVector_ 
	global $xVector_ $yVector_
	if {$tcl_version >= 8.0} {
	    $graph_ legend config -hide 1
	    if {![info exists $xVector_]} {
		blt::vector create $xVector_ $yVector_
	    }
	    set symbol {}
	} else {
	    $graph_ legend config -mapped 0
	    if {![info exists $xVector_]} {
		blt::vector $xVector_ $yVector_
	    }
	    set symbol none
	}
	$graph_ element create elem -xdata $xVector_ -ydata $yVector_ -symbol $symbol

	# plot the distribution of pixel values
	notify_cmd

	# add BLT features
        ::Blt_ZoomStack $graph_
        ::Blt_ActiveLegend $graph_
        ::Blt_Crosshairs $graph_
        ::Blt_ClosestPoint $graph_
	bind bltCrosshairs <Any-Motion> "catch {$this dispXY %x %y; %W crosshairs configure -position @%x,%y}"

	# Tk frame for X,Y positions.
	itk_component add fpos {
	    frame $w_.fpos -relief flat
	}
	# Tk label for X position.
	itk_component add xpos {
	    label $itk_component(fpos).xpos -width 20 -anchor w
	}
	# Tk label for Y position.
	itk_component add yval {
	    label $itk_component(fpos).yval -width 20 -anchor w
	}
	pack $itk_component(xpos) $itk_component(yval) -fill x -expand 0 -side left
	pack $itk_component(fpos) -fill none -expand 0

    }

   
    # make a hard copy of the graph display

    public method print {} {
	utilReUseWidget util::GraphPrint $w_.print \
	    -graph $graph_
    }
   
    # display x, y values at cursor position

    protected method dispXY {x y} {
	global $yVector_
	if {![$graph_ element closest $x $y "" -interpolate 1 -halo 10000]} {
	    return
	}
	lassign [$graph_ invtransform $x $y] x y
	set x [expr int(round($x))]
	if {$x < 1 || $x >= $numValues_} {
	    return
	}
	set yval [$yVector_ range $x $x]
	$itk_component(xpos) config -text "X: $x"
	$itk_component(yval) config -text "Value: $yval"
    }


    # make the button frame at the bottom of the window

    protected method make_buttons {} {
	pack [set b [frame $w_.buttons -borderwidth 2 -relief groove]] \
	    -side bottom -fill x

	pack \
	    [button $b.print -text "Print..." -command [code $this print]] \
	    [button $b.close -text "Close" -command [code $this quit]] \
	    -side left -expand 1 -padx 2m -pady 2m

	add_short_help $b.print \
	    {Print: Display a dialog window for printing the graph}
	add_short_help $b.close \
	    {Close: Close this window}
    }

    
    # quit the window

    public method quit {} {
	catch {$draw_ delete_object $itk_option(-line_id)}
	catch {destroy $w_}
    }

    
    # This method is called whenever the spectrum line is moved, resized
    # or deleted or when the image changed and the graph should be updated.
    # It updates the graph to show the image values along the line.

    public method notify_cmd {{op update}} {
	if {"$op" == "delete"} {
	    destroy $w_
	    return 0
	}
	lassign [$canvas_ coords $itk_option(-line_id)] x0 y0 x1 y1

	# plot the distribution of pixel values
	global $xVector_ $yVector_
	if {[catch {set numValues_ [$image_ spectrum $graph_ elem $x0 $y0 $x1 $y1 canvas \
		$xVector_ $yVector_]}]} {
	    return 0
	}
	$graph_ xaxis configure -max $numValues_
	return 0
    }


    # -- options --

    # name of RtdImage itcl widget, set by caller
    itk_option define -image image Image {} {
	# get internal widgets
	set canvas_ [$itk_option(-image) get_canvas]
	set image_ [$itk_option(-image) get_image]
	set draw_ [$itk_option(-image) component draw]
    }

    # canvas id of the spectrum line 
    itk_option define -line_id line_id Line_id {}

    # x0 canvas coordinate of the spectrum line
    itk_option define -x0 x0 X0 0

    # y0 canvas coordinate of the spectrum line
    itk_option define -y0 y0 Y0 0

    # x1 canvas coordinate of the spectrum line
    itk_option define -x1 x1 X1 0

    # y1 canvas coordinate of the spectrum line
    itk_option define -y1 y1 Y1 0

    # -- protected vars --
    
    # name of graph widget
    protected variable graph_

    # name of image's canvas widget
    protected variable canvas_

    # name of internal rtdimage object
    protected variable image_

    # name of RtdImage's CanvasDraw object
    protected variable draw_

    # number of values displayed
    protected variable numValues_ 0

    # x vector for graph
    protected variable xVector_ 
   
    # y vector for graph
    protected variable yVector_ 
}

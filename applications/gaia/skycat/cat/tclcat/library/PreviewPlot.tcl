# E.S.O. - VLT project
# "@(#) $Id: PreviewPlot.tcl,v 1.18 1998/11/20 14:19:35 abrighto Exp $"
#
# PreviewPlot.tcl - Widget for displaying a graph of tab table data
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  05 Aug 96  Created

itk::usual PreviewPlot {}

# PreviewPlot is an itcl widget for displaying a graph of tab table data, such
# as that returned from a "Preview" URL in catalogs such as the HST.

itcl::class cat::PreviewPlot {
    inherit util::TopLevelWidget

    #  constructor: create a new instance of this class

    constructor {args} {
	eval itk_initialize $args

	wm minsize $w_ 10 10
	wm title $w_ "$itk_option(-name) ($itk_option(-number))"

	# create astrocat object to manage tab table
	astrocat $w_.cat
	make_graph
	make_buttons
	make_short_help
	
	plot $itk_option(-file)
    }
    
 
    # destructor - clean up when deleted
    
    destructor {
	global ::tcl_version
	# delete the catalog object
 	catch {$w_.cat delete}
	if {$tcl_version >= 8.0} {
	    blt::vector destroy $xVector_ $yVector_
	}
   }

   
    # make the graph subwindow

    protected method make_graph {} {
	global ::tcl_version
	# note: make the graph in the global namespace for now so that
	# the old blt utils (features.tcl) still work. Shouldn't be needed
	# with blt-2.0 or later...
	set cmd \
	    [list blt::graph $w_.graph \
		 -width 8i \
		 -height 5i \
		 -borderwidth 3 \
		 -relief groove \
		 -title "Preview Data for $itk_option(-name)" ]
	# BLT graph widget
	itk_component add graph {
	    set graph_ [uplevel "#0" $cmd]
	} {
	}
	pack $itk_component(graph) \
	    -fill both -expand 1 -padx 1m -pady 1m

	add_short_help $itk_component(graph) \
	    {Graph: plot of preview data for object {bitmap dragb1} = zoom, {bitmap b2} = restore}
	
	# blt2.4f vector names ust start with a letter, no dots...
	# Also, they changed the default symbol to circle. Why?
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

	# add BLT features
        ::Blt_ZoomStack $graph_
        ::Blt_ActiveLegend $graph_
        ::Blt_Crosshairs $graph_
        ::Blt_ClosestPoint $graph_
	bind bltCrosshairs <Any-Motion> \
	    "catch {$this dispXY %x %y; %W crosshairs configure -position @%x,%y}"

	# X,Y position frame
	itk_component add fpos {
	    frame $w_.fpos -relief flat
	}
	# X position label.
	itk_component add xpos {
	    label $itk_component(fpos).xpos -width 20 -anchor w
	}
	# Y position label
	itk_component add yval {
	    label $itk_component(fpos).yval -width 20 -anchor w
	}
	pack $itk_component(xpos) $itk_component(yval) -fill x -expand 0 -side left
	pack $itk_component(fpos) -fill none -expand 0

    }

    # plot the data in the file 

    public method plot {file} {
	if {! [file exists $file]} {
	    error_dialog "no preview file was specified to plot"
	    return
	}
	if {[catch {$w_.cat open $file} msg]} {
	    error_dialog $msg $w_
	    return
	}
	set x {}
	set y {}
	lassign [$w_.cat headings] x y
	$graph_ xaxis configure -title $x
	$graph_ yaxis configure -title $y

	global $xVector_ $yVector_
	if {[catch {
	    set numValues_ [$w_.cat plot $graph_ elem $file $xVector_ $yVector_]
	} msg]} {
	    error_dialog "can't plot results: $msg"
	}

	# remove the temporary local catalog entry
 	$w_.cat entry remove $itk_option(-file)
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


    # make a hard copy of the graph display

    public method print {} {
	utilReUseWidget GraphPrint $w_.print -graph $graph_
    }


    # make the button frame at the bottom of the window

    protected method make_buttons {} {
	pack [set b [frame $w_.buttons -borderwidth 2 -relief groove]] \
	    -side top -fill x
	
	pack \
	    [button $b.print -text "Print..." -command [code $this print]] \
	    [button $b.close -text "Close" -command [code $this quit]] \
	    -side left -expand 1 -padx 2m -pady 2m
    }

    
    # quit the window

    public method quit {} {
	catch {destroy $w_}
    }

    
    # add a short help window and set the help texts
    
    protected method make_short_help {} {
	TopLevelWidget::make_short_help

	add_short_help $graph_ {Graph: drag {bitmap dragb1} to zoom in,  press {bitmap b3} to zoom back out}
	add_short_help $w_.buttons.print "Print: display a dialog window to print the graph \
                                         to a postscript file or printer."
	add_short_help $w_.buttons.close "Close this window."
    }


    # -- options --

    # file containing preview data in tab table format
    itk_option define -file file File {} 

    # name of object/star (for title)
    itk_option define -name name Name {} 

    # -- protected vars --
    
    # name of graph widget
    protected variable graph_

    # number of values displayed
    protected variable numValues_ 0

    # x vector for graph
    protected variable xVector_ 
   
    # y vector for graph
    protected variable yVector_ 
   
}

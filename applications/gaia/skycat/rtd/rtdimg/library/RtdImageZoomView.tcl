#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImageZoomView.tcl,v 1.19 1999/03/15 22:45:16 abrighto Exp $"
#
# RtdImageZoomView.tcl - itcl widget managing the RtdImage zoom window
#
# See man page RtdImageZoomView(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
#                 16 Sep 96  added code from P. Biereichel to clear
#                            zoom win when leaving window

itk::usual RtdImageZoomView {}

# This [incr Tk] widget class can be used to display a magnified portion
# of the image while tracking mouse motion events in the image window.
# There are two versions of this widget, see RtdImageZoom(n) for the
# other one. This version uses an rtdimage "view" of the main image and
# changes the x and y offsets as needed. This has the advantage that it
# always displays the correct pixels, even when the main image is
# "subsampled" and there are no restrictions on the size or shape of the
# zoom window. The main part of this widget is implemented in C++ by the
# rtdimage subcommand "zoomview".  This widget is a subclass of
# FrameWidget, so it inherits its methods and options. In addition the
# options and methods below are defined.

itcl::class rtd::RtdImageZoomView {
    inherit util::FrameWidget

    #  constructor: create a new instance of this class

    constructor {args} {
	itk_option add hull.borderwidth hull.relief
	global ::$w_.dozoom
	
	# evaluate arguments (the following code depends on them)
	eval itk_initialize $args

	# RtdImage(n) widget for zoom
	itk_component add image {
	    RtdImage $w_.image \
		-name "ZoomWin" \
		-verbose $itk_option(-verbose) \
		-displaymode 1 \
		-scrollbars 0 \
		-drag_scroll 0 \
		-show_object_menu 0 \
		-graphics 0 \
		-canvaswidth $itk_option(-width) \
		-canvasheight $itk_option(-height) \
		-usexshm $itk_option(-usexshm) \
                -usexsync $itk_option(-usexsync) \
		-shelp $itk_option(-shelp) \
		-relief groove \
		-borderwidth 2
		
	} {
	    rename -canvaswidth -width width Width
	    rename -canvasheight -height height Height
	}
	pack $itk_component(image) -side top -fill both -expand 1

	set image_ [$itk_component(image) get_image]
	set canvas_ [$itk_component(image) get_canvas]
	
	# draw a box around the center pixel
	$canvas_ create rectangle 0 0 1 1 \
	    -outline white -tags rect1
	$canvas_ create rectangle 0 0 1 1 \
	    -outline black -tags rect2
	
	# frame with on/off button and scale menu
	itk_component add f {
	    frame $w_.f
	} {
	    keep -background
	}
	pack $itk_component(f) -side bottom -fill both

	# checkbutton to turn zooming on/off
	itk_component add check {
	    checkbutton $w_.check \
		-text Zoom \
		-variable $w_.dozoom \
		-onvalue 1 -offvalue 0 \
		-anchor w \
		-borderwidth 2 -relief raised \
		-command [code $this zoom]
	} {
	    keep -background
	    rename -font -labelfont labelFont LabelFont
	}
	pack $itk_component(check) \
	    -side left -fill x -expand 1 -padx 0.5m -ipadx 0.5m -ipady 0.5m -in $w_.f

	# if the scale factor doesn't propagate automatically, add buttons to set it
	if {! $itk_option(-propagate)} {
	    # optional button to increase zoom factor
	    itk_component add larger {
		button $w_.larger \
		     -bitmap magnify \
		     -command [code $this inc_zoom 1]	
	    }
	    # optional button to decrease zoom factor
	    itk_component add smaller {
		button $w_.smaller \
		     -bitmap shrink \
		     -command [code $this inc_zoom -1]	
	    }
	    # optional label for zoom factor
	    itk_component add label {
		label $w_.label \
		     -text "" \
		     -width 3 \
		     -font $itk_option(-labelfont)
	    }
	    pack $itk_component(larger) $itk_component(smaller) $itk_component(label) \
		-side left -fill x -padx 0.5m -ipadx 0.5m -ipady 0.5m -in $w_.f

	    add_short_help $w_.larger \
		{Zoom larger: {bitmap b1} = increase magnification of zoom image}
	    add_short_help $w_.smaller \
		{Zoom smaller: {bitmap b1} = decrease magnification of zoom image}
	}

	# add help text displayed when mouse enters widget
	add_short_help $w_.check   {Zoom On/Off: {bitmap b1} = turn zooming on/off}
	add_short_help $w_ $itk_option(-shelp)
    }


    # increment or decrement the zoom factor

    public method inc_zoom {inc} {
	lassign [$image_ scale] xs
	if {"$xs" == ""} {
	    return
	}
	incr xs $inc
	if {$xs < 2} {
	    set xs 2
	} elseif {$xs > 50} {
	    set xs 50
	} 
	
	$image_ scale $xs $xs
	config -factor $xs
	scale 

	if {"$itk_option(-command)" != ""} {
	    eval $itk_option(-command)
	}
    }


    # called when the main image is scaled to draw a box around the center pixel.
    
    public method scale {} {
	
	if {$itk_option(-propagate)} {
	    set f [expr $target_scale_*$itk_option(-factor)]
	} else {
	    set f [lindex [$image_ scale] 0]
	    if {"$f" != ""} {
		$w_.label config -text "${f}x"
	    } else {
		# no image loaded...
		return
	    }
	}
	
	if {$target_scale_ <= 1} {
	    # box around pixel
	    set x0  [expr $itk_option(-width)/2.0]
	    set y0 [expr $itk_option(-height)/2.0]
	} else {
	    # box part of pixel
	    set x0  [expr $itk_option(-width)/2.0-$f/2.0]
	    set y0 [expr $itk_option(-height)/2.0-$f/2.0]
	}

	set x1  [expr $x0+$f]
	set y1 [expr $y0+$f]

	$canvas_ coords rect1 $x0 $y0 $x1 $y1
	$canvas_ coords rect2 [expr $x0-1] [expr $y0-1] [expr $x1+1] [expr $y1+1]
    }

    
    # This method is called when the mouse ptr enters an RtdImage.
    # Set the target scale factor from the given rtdimage
    
    public method enter_image {image} {
	lassign [$image scale] target_scale_
	if {$target_scale_ < 1} {
	    set target_scale_ 1
	}
	zoom
	scale
    }


    # This method is called when the mouse ptr leaves an RtdImage.
    # clear out the zoom image.
    
    public method leave_image {image} {
	$image_ clear ximage
	zoom 1
    }


    # called when the zoom checkbutton is pressed
    
    public method zoom {{clear 0}} {
	global ::$w_.dozoom
	if {[set $w_.dozoom] && ! $clear} {
	    catch {$target_image_ view remove $image_}
	    $target_image_ view add $image_ $itk_option(-propagate)
	    $target_image_ view update $image_ 0 0 $itk_option(-width) $itk_option(-height) 0 0 0 0 image
	    if {$itk_option(-propagate)} {
		$target_image_ zoomview start $image_ $itk_option(-factor) $itk_option(-propagate)
	    } else {
		$image_ scale $itk_option(-factor) $itk_option(-factor)
		$target_image_ zoomview start $image_ 1 0
	    }
	    scale
	} else {
	    $target_image_ zoomview stop
	    $image_ clear ximage
	    catch {$target_image_ view remove $image_}
	}
    }

    
    # -- options --

    # target (main) RtdImage itcl widget
    itk_option define -target_image target_image Target_image {} {
	set target_image_ [$itk_option(-target_image) get_image]
    }

    # width of zoom frame
    itk_option define -width width Width 152

    # height of zoom frame
    itk_option define -height height Height 152
   
    # zoom magnification factor
    itk_option define -factor factor Factor 4

    # flag: if true, make scale of zoom window relative to target window
    itk_option define -propagate propagate Propagate 1

    # X shared memory option
    itk_option define -usexshm useXshm UseXshm 1

    # X synchronisation option
    itk_option define -usexsync useXsync UseXsync 1

    # fonts used
    itk_option define -labelfont labelFont LabelFont {-Adobe-helvetica-bold-r-normal-*-12*}

    # help text displayed when mouse enters widget
    itk_option define -shelp shelp Shelp \
    	{Image Zoom: magnified section of image. {bitmap b1} = toggle on/off}

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0}

    # optional command to evaluate when the zoom factor changes
    itk_option define -command command Command {}

    # -- protected vars --

    # internal target image
    protected variable target_image_
    
    # scale of the target (or current target) image
    protected variable target_scale_ 1

    # internal canvas widget
    protected variable canvas_

    # internal rtd image
    protected variable image_
}

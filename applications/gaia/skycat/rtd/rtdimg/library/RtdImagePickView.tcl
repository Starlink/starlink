#*************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImagePickView.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# RtdImagePickView.tcl - itcl widget for zooming and computing statistics
#
# See the man page for a complete description.
#
# who       when      what
# --------  --------  ----------------------------------------------
# pbiereic  01/07/01  created
#
# RtdImagePickView handles a zoom window which is used to display 
# a magnified area of the main image at mouse pointer position
# (=mode zooming) and to compute the statistics on an area with a given
# sample size (=mode picking). The mode is set with method update_view.
# The area is surrounded by a rectangular box and can be changed
# with the option -ssize. The center coords for the statistics area
# are in chip coordinates.
#
# RtdImagePickView uses the second zoom view provided by RTD, so that the
# main zoom window should not be effected.
# 

itk::usual RtdImagePickView {}

itcl::class rtd::RtdImagePickView {
    inherit util::FrameWidget

    constructor {args} {
	eval itk_initialize $args

	itk_component add image {
	    rtd::RtdImage $w_.image \
		    -name           ZoomWin \
		    -scrollbars     0 \
		    -graphics       0 \
		    -canvaswidth    $cwidth_ \
		    -canvasheight   $cheight_ \
		    -borderwidth    0 \
		    -fillwidth      0 \
		    -fillheight     0 \
		    -shelp          $itk_option(-shelp)
	}
	catch {pack forget [$w_.image component hscrollf] \
		[$w_.image component vscrollf]}

	blt::table $w_ \
		$itk_component(image) 0,0 -fill both
	
	set image_  [$itk_component(image) get_image]
	set canvas_ [$itk_component(image) get_canvas]

	# add help text displayed when mouse enters widget
	add_short_help $w_ {Zoom Image: displays section of image being examined \
		{bitmap b2} = zoom in, \
		{bitmap b3} = zoom out}

	# disable configure events
	bind $canvas_ <Configure> {}

	# create a box to show the area for computing the statistics
	# (will be resized later)
	$canvas_ create rectangle 0 0 1 1 -outline white -tags rect1
	$canvas_ create rectangle 0 0 1 1 -outline black -tags rect2
	
	# add canvas bindings
	$canvas_ bind all <ButtonRelease-2> "+[code $this inc_zoom 1]"
	$canvas_ bind all <ButtonRelease-3> "+[code $this inc_zoom -1]"
    }

    # return canvas width and height (dimensions) of the visible image

    protected method get_canvas_dims { } {
	set cw [expr {double(min($cwidth_,  [$image_ width]  * $factor_))}]
	set ch [expr {double(min($cheight_, [$image_ height] * $factor_))}]
	return "$cw $ch"
    }

    # center the image

    protected method center { } {
	dbg "center"
	lassign [get_canvas_dims] cw ch

	# canvas scroll region is set to twice the canvas
	set xscroll [expr {0.5 - 0.25 * (1.0 - $cw / $cwidth_)}]
	set yscroll [expr {0.5 - 0.25 * (1.0 - $ch / $cheight_)}]

	$canvas_ xview moveto $xscroll
	$canvas_ yview moveto $yscroll
    }

    # increment or decrement the zoom factor

    public method inc_zoom { inc } {
	if { ! [checkInit] } { return }
	dbg "inc_zoom $inc"

	set f $factor_
	incr f $inc
	if {$f < 2} {
	    set f [max 2 $factor_]
	}
	set f [min $f 50]
	$image_ scale $f $f
	config -factor $f

	center       ; # center the image
	update_view  ; # update the view
	update_rect  ; # update the rectangular box

	if {"$itk_option(-command)" != ""} {
	    eval $itk_option(-command) $f
	}
    }

    # change the size of the zoom window (in canvas coords)

    public method change_size { cw ch } {
	set cwidth_ $cw
	set cheight_ $ch
	$canvas_ config -width $cwidth_ -height $cheight_
	$canvas_ config -scrollregion "-$cwidth_ -$cheight_ $cwidth_ $cheight_"
	center
    }

    # set the zoom factor

    public method set_scale { sx } {
	if { ! [checkInit] } { return }
	dbg "set_scale $sx"
	inc_zoom [expr {$sx - $factor_}]
    }

    # update the view of the image. If parameter $picking is 1 then
    # set the size of the view equal to the sample size (for computing
    # the statistics), else to the size of the visible image.

    public method update_view { { picking 0 } } {
	if { ! [checkInit] } { return }
	dbg "update_view $picking"

	lassign [cget -pickc] xc yc

	# "view update" requires offsets in canvas coords
	$target_image_ convert coords $xc $yc chip x0 y0 canvas

	# get scale factor of target image ("ti_" means "target image")
	$target_image_ convert dist 1 1 image ti_xs ti_ys canvas

	set f [expr {double($factor_)}]

	set iw [$image_ width]
	set ih [$image_ height]

	# compute offsets and image dimension in canvas coords:
	# - when picking compute coords for the sample area.
	# - when zooming compute coords for the zoom window.

	if { $picking } {
	    # compute values for updating the view for the sample area
	    set ssize [expr {double([cget -ssize])}]
	    set cw [expr {$ssize * $f}]
	    set ch [expr {$ssize * $f}]
	    set nx $ssize
	    set ny $ssize
	} else {
	    # compute values for updating the view for the zoom image
	    lassign [get_canvas_dims] cw ch
	    set nx [expr {$cw / $f}]
	    set ny [expr {$ch / $f}]
	}

	# compute offsets in target image (canvas coords)
	set x0 [expr {$x0 - $nx / 2.0 * $ti_xs}]
	set y0 [expr {$y0 - $ny / 2.0 * $ti_ys}]

	# update the view
	$target_image_ view update $image_ $x0 $y0 \
	    [expr {$cw * $ti_xs}] [expr {$ch * $ti_ys}] 0 0 0 0 canvas
    }

    # compute statistics on the image currently viewed

    public method statistics { xc yc } {
	if { ! [checkInit] } { return }
	dbg "statistics $xc $yc"

	config -pickc "$xc $yc"
	update_view 1  ; # set picking mode

	# compute statistics on the image currently viewed
	if { [catch {$image_ statistics} result] } {
	    error_dialog $result
	    return {}
	}
	# convert x,y to chip coords
	$target_image_ convert coords [lindex $result 0] [lindex $result 1] image \
		xc yc chip
	set result [lreplace $result 0 1 $xc $yc]

	return $result
    }

    # move the zoom view to xc, yc

    public method moveTo { xc yc } {
	dbg "moveTo $xc $yc"
	config -pickc "$xc $yc"
	$image_ scale $factor_ $factor_
	center
	update_view
    }

    # display a rectangular box showing the area for computing
    # the statistics

    public method update_rect { } {
	if { ! [checkInit] } { return }
	dbg "update_rect"

	set hsize [expr {double([cget -ssize]) / 2.0}]

	lassign [get_canvas_dims] cw ch
	set xc [expr {$cw / $factor_ / 2.0}]
	set yc [expr {$ch / $factor_ / 2.0}]

        $image_ convert dist [expr {$xc - $hsize}] [expr {$xc + $hsize}] image x0 x1 canvas
        $image_ convert dist [expr {$yc - $hsize}] [expr {$yc + $hsize}] image y0 y1 canvas

	$canvas_ coords rect1 $x0 $y0 $x1 $y1
	$canvas_ coords rect2 \
	    [expr {$x0 - 1.1}] [expr {$y0 - 1.1}] [expr {$x1 + 1.1}] [expr {$y1 + 1.1}]
    }

    # activate / de-activate zoom 

    public method activate { bool } {
	if { $bool == $activated_ } { return }
	dbg "activate $bool"
	if { $bool } {
	    # add view without propagating the scale factor of the main image
	    $target_image_ view add $image_ 0
	} else {
	    $target_image_ view remove $image_
	}
	set activated_ $bool
    }

    # start / stop zoom 

    public method zoom { bool } {
	if { $bool == $zooming_ } { return }
	dbg "zoom $bool"
	if { $bool } {
	    if { ! $activated_ } { return }
	    # start zoomview: scale 1, don't propagate scale factor, 2'nd zoom
	    $target_image_ zoomview start $image_ 1 0 $zoomNr_
	    # set scale, update view and rectangular box
	    inc_zoom 0
	} else {
	    $target_image_ zoomview stop $zoomNr_
	}
	set zooming_ $bool
    }

    # clear zoom image

    public method clear { } {
	dbg "clear"
	$image_ clear ximage
    }

    # check that this widget is properly initialized.
    # Returns 1 for yes and 0 for no.

    protected method checkInit { } {
	lassign [cget -pickc] xc yc
	if { "$xc" == "" || "$yc" == "" || "[cget -ssize]" == "" || \
		[$image_ isclear] } {
	    return 0
	}
	return 1
    }

    # return name of the rtdimage object

    public method get_image { } {
	return $image_
    }

    # return canvas pathname of zoom image

    public method get_canvas { } {
	return $canvas_
    }

    # return scale factor

    public method get_scale { } {
	return $factor_
    }

    # debug for development

    protected method dbg { msg } {
	return
	puts "RtdImagePickView: $msg"
    }
    
    # -- options --

    # target (main) RtdImage itcl widget
    itk_option define -target_image target_image Target_image {} {
	set target_image_ [[cget -target_image] get_image]
    }

    # help text when mouse enters the widget
    itk_option define -shelp shelp Shelp {}

    # eval command after scale changed
    itk_option define -command command Command  {}

    # default zoom magnification factor
    itk_option define -factor factor Factor {4} {
	set factor_ [cget -factor]
    }

    # sample size for computing statistics
    itk_option define -ssize ssize Ssize {} {
	set ssize [cget -ssize]
	if { ! [lempty $ssize] } {
	    dbg "config -ssize [cget -ssize]"
	    update_rect
	}
    }

    # picked target image coords
    itk_option define -pickc pickc Pickc {}

    # -- protected vars --

    protected variable target_image_ ; # target image
    protected variable canvas_       ; # ZoomView's canvas
    protected variable image_        ; # ZoomView's canvas image tag

    protected variable zoomNr_ 2     ; # use the second view zoom of RtdImage
    protected variable cwidth_  185  ; # default width of zoom frame
    protected variable cheight_ 185  ; # default height of zoom frame
    protected variable activated_ 0  ; # ZoomView activated, bool
    protected variable zooming_ 0    ; # zooming started / stopped, bool

    # Scale factor: we cannot rely on the scale factor of the image
    # since the main image propagates always the scale factor when there was
    # a new image.
    protected variable factor_ 4     ; # scale factor
}

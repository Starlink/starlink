# E.S.O. - VLT project 
# "@(#) $Id: RtdImageMBand.tcl,v 1.1.1.1 2006/01/12 16:38:23 abrighto Exp $"
#
# RtdImageMBand.tcl - itcl class to display a "measure band" 
#                     showing the distance between 2 points in world coordinates
#
# See man page RtdImagMBand(n) for a complete description.
#
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created

itk::usual RtdImageMBand {}

# RtdImageMBand is an itcl widget class used to display a "measure band" 
# showing the distance between two points in world coordinates.

itcl::class rtd::RtdImageMBand {
    inherit util::FrameWidget

    # create a new object (the frame only ensures that this class
    # will be deleted along with its parent)

    constructor {args} {
	eval itk_initialize $args

	set image_ [$itk_option(-image) get_image]
	set canvas_ [$itk_option(-image) get_canvas]
	
	bind $canvas_ <3> "+[code $this start %x %y]"
    }

    
    # start displaying the measure band

    public method start {x y} {
	$canvas_ delete mband

	if {"[$image_ wcscenter]" == ""} {
	    return
	}

	set x_ $x
	set y_ $y
	set x [$canvas_ canvasx $x]
	set y [$canvas_ canvasy $y]
	
	# create diagonal line with arrows
	$canvas_ create line $x $y $x $y \
	    -width $itk_option(-line_width) \
	    -fill $itk_option(-line_color) \
	    -arrow $itk_option(-arrow_type) \
	    -arrowshape $itk_option(-arrow_shape) \
	    -tags {mband mband_line}

	# dashed angle line shows width and height
	$canvas_ create line $x $y $x $y $x $y \
	    -width $itk_option(-line_width) \
	    -fill $itk_option(-line_color) \
	    -stipple  pat8 \
	    -tags {mband mband_angle}
	
	# create labels for width, height, diagonal
	foreach i {width height diag} {
	    $canvas_ create rectangle $x $y [expr {$x+1}] [expr {$y+1}] \
		-fill $itk_option(-fill_color) \
		-outline $itk_option(-outline_color) \
		-tags "mband mband_${i}_rect"

	    $canvas_ create text $x $y \
		-fill $itk_option(-text_color) \
		-font $itk_option(-text_font) \
		-anchor w \
		-tags "mband mband_${i}_text"
	}

	# save and set cursor
	$canvas_ config -cursor $itk_option(-cursor)

	# save and set bindings
	set saved_bindtags_ [bindtags $canvas_]
	# use a unique name 
	set tag mband$w_
	bind $tag <3> [code $this stop]
	bind $tag <ButtonRelease-3> [code $this check_stop %x %y]
	bind $tag <Motion> [code $this mband %x %y 1]
	bind $tag <Shift-Motion> { }
	bind $tag <Control-Motion> [code $this mband %x %y 0]
	bind $tag <Shift-B3-Motion> [code $this mband %x %y 0]
	#bind $tag <Control-B3-Motion> [code $this mband %x %y 0]
	bind $tag <Control-B3-Motion> { }

	# make arrow keys move mouse pointer by one pixel
	bind $tag <Left> "$image_ warp -1 0"
	bind $tag <Right> "$image_ warp 1 0"
	bind $tag <Up> "$image_ warp 0 -1"
	bind $tag <Down> "$image_ warp 0 1"

	bindtags $canvas_ $tag
    }

    
    # stop displaying the measure band and restore cursor and bindings

    public method stop {} {
	$canvas_ delete mband
	$canvas_ config -cursor $itk_option(-defaultcursor)
	bindtags $canvas_ $saved_bindtags_
    }


    # stop displaying the mband if the user has moved the mouse since it was
    # created

    public method check_stop {x y} {
	if {$x != $x_ && $y != $y_} {
	    stop
	}
    }


    # update the display of the measure band to show the distance
    # between the endpoints in WCS

    public method mband {x y show_angle} {
	$image_ mband $x_ $y_ $x $y screen $show_angle
	update idletasks
    }


    # -- public vars --

    # main RtdImage widget (set by caller)
    itk_option define -image image Image {}
    
    # line width option for measure band
    itk_option define -line_width line_width Line_width 1

    # line color option for measure band
    itk_option define -line_color line_color Line_color white

    # line arrow type option for measure band
    itk_option define -arrow_type arrow_type Arrow_type both

    # line arrow shape option for measure band
    itk_option define -arrow_shape arrow_shape Arrow_shape {8 10 3}

    # outline color for label rectangle
    itk_option define -outline_color outline_color Outline_color {grey90} 

    # fill color for label rectangle
    itk_option define -fill_color fill_color Fill_color {yellow}

    # text color for label
    itk_option define -text_color text_color Text_color {blue}

    # font to use for labels
    itk_option define -text_font text_font Text_font {-*-courier-bold-r-*-*-*-100-*-*-*-*-*-*}

    # default cursor when drawing
    itk_option define -cursor cursor Cursor {draft_small}

    # default cursor when not drawing
    itk_option define -defaultcursor defaultCursor DefaultCursor {}

    # -- protected vars -- 

    # internal rtdimage widget for main image
    protected variable image_

    # canvas window for image
    protected variable canvas_

    # X starting point of line
    protected variable x_ 0

    # Y starting point of line
    protected variable y_ 0

    # saved canvas bindings, restored later
    protected variable saved_bindtags_ 
}

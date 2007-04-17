#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImageZoom.tcl,v 1.1.1.1 2006/01/12 16:38:09 abrighto Exp $"
#
# RtdImageZoom.tcl - itcl widget managing the RtdImage zoom window
# 
# See man page RtdImageZoom(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created


itk::usual RtdImageZoom {}

# Note: It is better to use the RtdImageZoomView class, since this
# class is outdated and being phased out.
#
# This [incr Tk] widget class can be used to display a magnified portion
# of the image while tracking mouse motion events in the image window.
# There are two versions of this widget, see RtdImageZoomView(n) for the
# other one. This version takes a caller supplied Tk frame and zooms
# directly from the main image's XImage to the frame, but does not
# display an accurate image when the main image is "subsampled"
# (shrunk).
#
# The main part of this widget is implemented in C++ by the
# rtdimage subcommand "zoom".
#
# This widget is a subclass of FrameWidget, so it inherits its
# methods and options. In addition the options and methods below
# are defined.

itcl::class rtd::RtdImageZoom {
    inherit util::FrameWidget

    #  constructor: create a new instance of this class

    constructor {args} {
	itk_option add hull.borderwidth hull.relief
	eval itk_initialize $args
	# zoom frame
	itk_component add frame {
	    set zoom_ [frame $w_.frame \
			     -background black \
			     -borderwidth 3 \
			     -relief groove \
			     -width $itk_option(-width) \
			     -height $itk_option(-height)]
	} {
	}
	pack $itk_component(frame) -side top

	global ::$w_.dozoom
	# checkbutton to turn zoom on/off
	itk_component add check {
	    checkbutton $w_.check -text Zoom \
		  -variable $w_.dozoom \
		  -onvalue 1 -offvalue 0 \
		  -anchor w \
		  -borderwidth 2 \
		  -relief raised \
		  -command [code $this zoom]
	} {
	}
	pack $itk_component(check) -side bottom -fill both

	# add help text displayed when mouse enters widget
	add_short_help $w_ $itk_option(-shelp)
    }


    # called when the zoom checkbutton is pressed
    
    public method zoom {} {
	global ::$w_.dozoom
	if {[winfo width $zoom_] <= 1} {
	    update
	}
	if {[set $w_.dozoom]} {
	    $itk_option(-target_image) zoom start $zoom_ $itk_option(-factor)
	} else {
	    $itk_option(-target_image) zoom stop
	}
    }


    # called when the main image is scaled. 
    
    public method scale {} {
    }


    # This method is called when the mouse ptr enters an RtdImage.
    # Set the target scale factor from the given rtdimage
    
    public method enter_image {image} {
    }


    # This method is called when the mouse ptr leaves an RtdImage.
    # clear out the zoom image.
    
    public method leave_image {image} {
    }

    
    # -- public vars --

    # target RtdImage itcl widget
    itk_option define -target_image target_image Target_image {}

    # width of zoom frame
    itk_option define -width width Width 128

    # height of zoom frame
    itk_option define -height height Height 128
   
    # zoom factor (window size should be a multiple of this)
    itk_option define -factor factor Factor 4
    
    # help text displayed when mouse enters widget
    itk_option define -shelp shelp Shelp \
	{Image Zoom: magnified section of image. {bitmap b1} = toggle on/off}

    # X shared memory option
    itk_option define -usexshm useXshm UseXshm 1

    # X synchronisation option
    itk_option define -usexsync useXsync UseXsync 1

    # -- protected vars --

    # internal zoom frame
    protected variable zoom_
}
 

#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImageFrame.tcl,v 1.1.1.1 2006/01/12 16:38:23 abrighto Exp $"
#
# RtdImageFrame.tcl - itcl widget for displaying a section of an rtdimage 
# at a given position in a canvas window
# 
# See man page RtdImageFrame(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
#                 23 Jun 96  assume rapid frame data has separate mem area
#                            


itk::usual RtdImageFrame {}

# This widget is used to display rapid frames for an RtdImage widget. A
# rapid frame is an instance of a RtdImage widget that displays a small
# section of the main image and can be updated faster with real-time
# images because it is smaller than the main image.  
#
# The area in the main image being used for the rapid frame is marked
# with one black and one white dashed rectangle. The rapid frame can be
# moved or resized in in the same way as any other graphic objects by
# dragging with the left mouse button over it or on one of the 8 resize
# handles displayed around it when it is selected. One of the dashed
# rectangles shows the current position of the rapid frame while the
# other one shows the new position and size being set.
#
# Two types of rapid frames are supported.  The first type is an
# RtdImage "view" of the main image embedded into its canvas window at a
# given x,y offset, so that it appears that main image is being updated
# more frequently inside the dashed box. In this case, the rapid frame
# is a separate rtdimage canvas image item in the same canvas with the
# main image, but at a different offset.  The second type of rapid frame
# is displayed in a popup window and is implemented by the class
# RtdImagePopup(n).
#
# Creating and manipulating a rapid frame usually involves communication
# with the rtdServer and camera, to tell the camera to start sending
# images at the given rate from the given area. Since this is very
# application specific, you can arrange to have your own Tcl command
# evaluated whenever a rapid frame is created, moved, resized or
# deleted. See the RtdImage(n) -rapid_frame_command option for how to do
# this.
#
# Note that currently, only one rapid frame is allowed at a time. 
# Creating a second one automatically deletes the first. This may be
# changed in a future release.

itcl::class rtd::RtdImageFrame {
    inherit util::FrameWidget

    #  constructor: create a new instance of this class

    constructor {args} {
	eval itk_initialize $args
    }

    # this method is called from the base class (FrameWidget) after all
    # the options have been evaluated

    protected method init {} {
	# put image in target canvas
	# note: create in global namespace to avoid problems in C++ side
	set cmd \
	    [list image create rtdimage \
		 -name "rapidFrame" \
		 -displaymode 1 \
		 -usexshm $itk_option(-usexshm) \
                 -usexsync $itk_option(-usexsync) \
		 -verbose $itk_option(-verbose) \
		 -subsample $itk_option(-subsample)]
	set image_ [uplevel "#0" $cmd]

	set imageId_ [$canvas_ create image $itk_option(-xoffset) $itk_option(-yoffset) \
			  -image $image_ \
			  -anchor nw \
			  -tags $image_]
	
	# make sure the stacking order is correct
	$canvas_ raise $image_ $target_image_
	$canvas_ raise $itk_option(-region_id) $image_

	# create a dummy rect to get events for image
	set rectId_ [$canvas_ create rectangle \
			 $itk_option(-xoffset) $itk_option(-yoffset) \
			 [expr {$itk_option(-xoffset)+$itk_option(-width)-1}] \
			 [expr {$itk_option(-yoffset)+$itk_option(-height)-1}] \
			 -tags imagerect \
			 -fill black \
			 -stipple pat7]
	# add bindings for moving and resizing the rapid frame
	$draw_ add_object_bindings $rectId_ $itk_option(-region_id)

	$target_image_ view add $image_ 1 1
	set frameId_ [$image_ frameid]
	
	# setup callbacks for moving and resizing image
	$draw_ add_notify_cmd $itk_option(-region_id) [code $this notify_cmd]
	notify_cmd resize

	# handle interaction between zoom window and rapid frame
	$canvas_ bind $rectId_ <Any-Enter> "+[code $target_image_ view enter $image_]"
	$canvas_ bind $rectId_ <Any-Leave> "+[code $target_image_ view leave $image_]"

	$image_ config -newimagecmd [code $this notify_cmd resize]
    }


    # return the name of the underlying rtdimage object
    
    public method get_image {} {
	return $image_
    }


    # destructor - clean up when deleted
    
    destructor {
	$canvas_ delete $imageId_
	image delete $image_
	$draw_ remove_notify_cmd $itk_option(-region_id)
	$draw_ delete_object $rectId_
	$draw_ delete_object $itk_option(-region_id)
    }

    
    # This method is called (from the main image's CanvasDraw(n) widget) 
    # whenever an embedded rapid frame is moved, resized or deleted. 
    # If the "-command" option was given to this class, then that tcl command is
    # evaluated with the frameId, operation name (move, resize, delete) the x, y coords
    # and the width and height of the frame.

    public method notify_cmd {op args} {
	if {"$op" == "delete"} {
	    if {"$itk_option(-command)" != ""} {
		eval "$itk_option(-command) $frameId_ $this $op 0 0 0 0"
	    }
	    delete object $this
	    return 0
	}
	lassign [$canvas_ bbox $itk_option(-region_id)] x0 y0 x1 y1
	set w [expr {$x1-$x0+1}]
	set h [expr {$y1-$y0+1}]
	$canvas_ coords $imageId_ $x0 $y0
	$canvas_ coords $rectId_ $x0 $y0 $x1 $y1
	
	# note: we assume here that the rapid frame has its own memory area 
	# starting at 0,0
	$target_image_ view update $image_ 0 0 $w $h $x0 $y0 0 0 canvas
	
	if {"$itk_option(-command)" != ""} {
	    eval "$itk_option(-command) $frameId_ $this $op $x0 $y0 $w $h"
	}
    }

    
    # -- public vars --
    
    # target rtdimage
    itk_option define -target_image target_image Target_image {} {
	# get internal widget names for target image
	set target_image_ [$itk_option(-target_image) get_image]
	set canvas_ [$itk_option(-target_image) get_canvas]
	set draw_ [$itk_option(-target_image) component draw]
    }

    # canvas id of the (region) object used to position and move the image 
    # in the canvas
    itk_option define -region_id region_id Region_id {}

    # X offset of image frame
    itk_option define -xoffset xoffset Xoffset 0

    # Y offset of image frame
    itk_option define -yoffset yoffset Yoffset 0

    # Width of image frame
    itk_option define -width width Width 0

    # Height of image frame
    itk_option define -height height Height 0
    
    # tcl command to be evaluated whenever the frame is created moved, 
    # resized or deleted: 7 args will be appended: 
    #  
    #  frameId = unique rapid frame id for use with rtdServer
    #
    #  name = unique name for the frame
    #
    #  op  = {move,resize or delete}
    #
    #  x, y = coords of upper left corner of frame in image 
    # 
    #  width, height = dimensions of frame.
    itk_option define -command command Command {}

    # flag: if true, pan image is "subsampled" when shrinking, 
    # otherwise the pixels are averaged
    itk_option define -subsample subsample Subsample 1
   
    # X shared memory option
    itk_option define -usexshm useXshm UseXshm 1

    # X synchronisation option
    itk_option define -usexsync useXsync UseXsync 1

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0}

    # -- protected vars --
    
    # target internal rtdimage
    protected variable target_image_

    # CanvasDraw object, for setting up move, resize operations on embedded image
    protected variable draw_

    # internal rtdimage for rapid frame
    protected variable image_

    # canvas window containing rapid frame image 
    protected variable canvas_
    
    # canvas image id
    protected variable imageId_

    # canvas id of rectangle used to get events for moving/resizing image
    protected variable rectId_

    # rapid frame Id, needed to communicate with rtdServer
    protected variable frameId_
}

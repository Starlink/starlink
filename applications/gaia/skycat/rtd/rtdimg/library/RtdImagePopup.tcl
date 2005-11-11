#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImagePopup.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# RtdImagePopup.tcl - A toplevel widget for displaying rapid frames for RtdImage
# 
# See man page RtdImagePopup(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  13 Mar 96  Created
#                 23 Jun 96  assume rapid frame data has separate mem area
#                            starting at 0,0

itk::usual RtdImagePopup {}

# This widget is used to display rapid frames in a popup window for an
# RtdImage widget. A rapid frame is an instance of a RtdImage widget
# that displays a small section of the main image and can be updated
# faster with real-time images because it is smaller than the main
# image.
#
# The area in the main image being used for the rapid frame is marked
# with one black and one white dashed rectangle. The rapid frame can be
# moved or resized in in the same way as any other graphic objects by
# dragging with the left mouse button over it or on one of the 8 resize
# handles displayed around it when it is selected. One of the dashed
# rectangles shows the current position of the rapid frame while the
# other one shows the new position and size being set.
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
# Creating a second one automatically deletes the first. This may
# be changed in a future release.

itcl::class rtd::RtdImagePopup {
    inherit util::TopLevelWidget

    #  constructor: create a new instance of this class

    constructor {args} {
	eval itk_initialize $args
    }


    # add bindings and callbacks after the constructor was called

    protected method init {} {
	wm title $w_ "Rapid Frame ($itk_option(-number))"
	wm iconname $w_ Rapid

	# RtdImage(n) widget to display in a popup window
	itk_component add image {
	    rtd::RtdImage $w_.image \
		-name "RapidFrame" \
		-canvasheight $itk_option(-height) \
		-displaymode 1 \
		-graphics 0 \
		-drag_scroll 0 \
		-zoomwin $itk_option(-zoomwin) \
		-usexshm $itk_option(-usexshm) \
		-verbose $itk_option(-verbose) \
		-subsample $itk_option(-subsample) \
		-scrollbars 1 \
		-newimagecmd [code $this new_image_cmd]
	}

	set canvas_ [$itk_component(image) get_canvas]
	set image_ [$itk_component(image) get_image]

	add_menubar
	make_panel
	
	pack $itk_component(image) -fill both -expand 1
	
	# create a dummy rect to get events for image
	set rectId_ [$target_canvas_ create rectangle \
			 $itk_option(-xoffset) $itk_option(-yoffset) \
			 [expr {$itk_option(-xoffset)+$itk_option(-width)-1}] \
			 [expr {$itk_option(-yoffset)+$itk_option(-height)-1}] \
			 -tags imagerect \
			 -fill black \
			 -stipple pat7]

	# set help text displayed when mouse enters widget
	add_short_help $canvas_ $itk_option(-shelp)

	$target_image_ view add $image_ 0 1
	set frameId_ [$image_ frameid]
	
	# RtdImageMBand(n) widget: 
	# The "measure band" is displayed while the right mouse button 
	# is pressed to show the distance between points.
	itk_component add mband {
	    RtdImageMBand $w_.mband -image $itk_component(image)
	} 
	
	bind $w_ <Configure> [code $itk_component(image) center]

	# handle interaction between zoom window and rapid frame
	$canvas_ bind [$itk_component(image) get_imageId] <Any-Enter> \
	    "+[code $target_image_ view enter $image_]"
	$canvas_ bind [$itk_component(image) get_imageId] <Any-Leave> \
	    "+[code $target_image_ view leave $image_]"

	notify_cmd resize

	# add bindings for moving and resizing the rapid frame
	$draw_ add_object_bindings $rectId_ $itk_option(-region_id)

	# setup callbacks for moving and resizing image
	$draw_ add_notify_cmd $itk_option(-region_id) [code $this notify_cmd]
    }


    # destructor - clean up when deleted
    
    destructor {
	$draw_ remove_notify_cmd $itk_option(-region_id)
	$draw_ delete_object $rectId_
	$draw_ delete_object $itk_option(-region_id)
    }


    # return the name of the underlying rtdimage object
    
    public method get_image {} {
	return $image_
    }


    # add the menubar at the top of the window

    protected method add_menubar {} {
	# menu bar
	TopLevelWidget::add_menubar

	set image $itk_component(image)
	

	# File menu
	set m [add_menubutton File]
	add_menuitem $m command "Save as..." \
	    {Save the current image to a (FITS) file} \
	    -command [code $image save_as]

	add_menuitem $m command "Print..." \
	    {Print the current image to a file or printer} \
	    -command [code $image print]

	#add_menuitem $m command Clear \
	#    {Clear the image display} \
	#    -command [code $image clear]

	add_menuitem $m command "Close" \
	    {Close the window} \
	    -command "destroy $w_"

	add_short_help $itk_component(menubar).file \
	    {File menu: save, clear, print image, close window}


	# View menu
	set m [add_menubutton View]
	add_menuitem $m command "Cut Levels..." \
	    {Display a window for manipulating the image cut levels} \
	    -command [code $w_.info cut_level_dialog]

	add_short_help $itk_component(menubar).view \
	    {View menu: manipulate image cut levels}
    }

    
    # make the upper panel 

    protected method make_panel {} {
	# RtdImagePanel(n) widget, control panel
	itk_component add info {
	    RtdImagePanel $w_.info \
		-image $itk_component(image) \
		-showobject 0 \
		-showxy 0 \
		-showwcs 0 \
		-showminmax 0 \
		-shorthelpwin $itk_option(-shorthelpwin) \
		-state normal \
		-min_scale $itk_option(-min_scale) \
		-max_scale $itk_option(-max_scale) \
		-borderwidth 3 -relief groove
	}
	pack $itk_component(info)  \
	    -side top -fill x
	
    }

    
    # set the cut levels

    public method set_cut_levels {} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	utilReUseWidget rtd::RtdImageCut $w_.cut \
	    -image $this \
	    -transient 1 \
	    -command "$itk_component(info) updateValues"
    }


    # this method is called by the image code whenever a new image is loaded
    # (for updates, see camera command)

    protected method new_image_cmd {} {
	$itk_component(info) updateValues	
    }
    

    # This method is called (from the main image's CanvasDraw(n)
    # widget) whenever an embedded rapid frame is moved, resized
    # or deleted. 
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
	} elseif {"$op" == "flip"} {
	    eval $itk_component(image) flip $args
	    $itk_component(info) component trans update_trans
	} elseif {"$op" == "rotate"} {
	    eval $itk_component(image) rotate $args
	    $itk_component(info) component trans update_trans
	    after idle [code $itk_component(image) center]
	} elseif {"$op" == "resize" || "$op" == "move"} {
	    lassign [$target_canvas_ bbox $itk_option(-region_id)] x0 y0 x1 y1
	    set w [expr {$x1-$x0+1}]
	    set h [expr {$y1-$y0+1}]
	    $target_canvas_ coords $rectId_ $x0 $y0 $x1 $y1

	    # note: we assume here that the rapid frame has its own memory area 
	    # starting at 0,0
	    $target_image_ view update $image_ 0 0 $w $h 0 0 $x0 $y0 canvas
	    
	    if {"$itk_option(-command)" != ""} {
		eval "$itk_option(-command) $frameId_ $this $op $x0 $y0 $w $h"
	    }
	}
	after idle [code $itk_component(image) center]
    }

    
    # -- public vars --
    
    # target rtdimage
    itk_option define -target_image target_image Target_image {} {
	# get internal widget names for target image
	set target_image_ [$itk_option(-target_image) get_image]
	set target_canvas_ [$itk_option(-target_image) get_canvas]
	set draw_ [$itk_option(-target_image) component draw]
    }

    # canvas id of the (region) object used to position and move the image 
    # in the canvas
    itk_option define -region_id region_id Region_id {}

    # X offset of image frame
    itk_option define -xoffset xoffset Xoffset 0

    # Y offset of image frame
    itk_option define -yoffset yoffset Yoffset 0

    # width of image frame
    itk_option define -width width Width 0

    # height of image frame
    itk_option define -height height Height 0
    
    # This tcl command is evaluated whenever the frame is created
    # moved, resized or deleted: 7 arguments will be appended to the
    # command before it is evaluated:
    #
    # frameId: Unique rapid frame id for use with rtdServer.
    #
    # name: Unique name for the frame.
    #
    # op: Operation: one of: move,resize or delete. 
    #
    # x, y: Coordinates of upper left corner of frame in main image.
    #
    # width, height: Dimensions of rapid frame.
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

    # zoom window to update
    itk_option define -zoomwin zoomWin ZoomWin {}

    # text of short help message to be displayed whenever
    # the mouse enters the image window (see Toplevel.tcl)
    itk_option define -shelp shelp Shelp "image window: {bitmap b1} = select graphics, \
                 {bitmap b2} = scroll image, \
                 {bitmap dragb3} = measure world coordinates"

    # optionally specify TopLevelWidget to display short help messages
    itk_option define -shorthelpwin shortHelpWin ShortHelpWin {}

    # minimum allowed scale value
    itk_option define -min_scale min_scale Min_scale -10

    # maximum allowed scale value
    itk_option define -max_scale max_scale Max_scale 20

    # -- protected vars --
    
    # target internal rtdimage
    protected variable target_image_ 

    # canvas widget for main image and region object marking frame.
    protected variable target_canvas_ 

    # CanvasDraw object, for setting up move, resize operations on embedded image
    protected variable draw_

    # internal rtdimage for rapid frame
    protected variable image_

    # canvas window containing rapid frame image, 
    # different than target_canvas for popup frames
    protected variable canvas_
    
    # canvas id of rectangle used to get events for moving/resizing image
    protected variable rectId_

    # rapid frame Id, needed to communicate with rtdServer
    protected variable frameId_
}

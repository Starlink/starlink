#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImagePan.tcl,v 1.1.1.1 2006/01/12 16:38:07 abrighto Exp $"
#
# RtdImagePan.tcl - itcl widget managing the RtdImage panning window
# 
# See man page RtdImagePan(n) for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
# Peter W. Draper 15 Feb 01  Changed notify_cmd method to deal with
#                            very narrow images (spectra).
# pbiereic        14/12/04   Fixed: Panning while image events are received
# Peter W. Draper 02 Apr 05  Slight correction to logic of above. Make sure
#                            panning changes are always seen for new images 
#                            and images with orientation changes (pan with 
#                            changed=1)
#                 02 Nov 06  Make the compass a fixed size of the width, not
#                            some size that depends on the image scale. More
#                            consistent.
# pbiereic        30/03/05   Fixed: pan image width for long, narrow spectra
#                            in method notify_cmd

itk::usual RtdImagePan {}

# This widget displays a "view" of another RtdImage widget at a
# smaller magnification, so that the entire image is visible in
# a small window. A rectangle displayed over the image can be
# used to pan or move the image when the target image is to
# large to be viewed at all at once in its window. The rectangle
# is always notified of changes in the target image or its
# window, so it always displays the relative size of the visible
# image to the entire image. The pan window is based on the
# rtdimage "pan" subcommand and uses canvas graphics to display
# the rectangle.
#
# Since it is not known ahead of time how large or small an
# image will be, the pan window is given a maximum size when
# created. When an image is loaded, it shrinks the image by
# equal integer factors until it fits in the window. Then it
# fits the window around the image, so as not to leave a blank
# (black) space around it. Rotated and flipped images are also
# displayed as rotated and flipped in the pan window. Only the
# scale factor remains fixed.

itcl::class rtd::RtdImagePan {
    inherit util::FrameWidget

    # constructor: create a new RtdImagePan widget

    constructor {args} {
	itk_option add hull.borderwidth hull.relief

	# evaluate the options
	eval itk_initialize $args

	# RtdImage(n) widget for displaying a copy of the image
	itk_component add image {
	    rtd::RtdImage $w_.image \
		-name "Panner" \
		-scrollbars 0 \
		-drag_scroll 0 \
		-displaymode 0 \
		-show_object_menu 0 \
	        -withtoolbox 0 \
	        -graphics 1 \
		-fitwidth $itk_option(-width) \
		-fitheight $itk_option(-height) \
		-subsample $itk_option(-subsample) \
		-shelp $itk_option(-shelp) \
		-zoomwin $itk_option(-zoomwin) \
		-cursor $itk_option(-cursor) \
		-verbose $itk_option(-verbose) \
                -usexsync $itk_option(-usexsync) \
		-usexshm $itk_option(-usexshm) \
	} {
	    keep -subsample -zoomwin -cursor -verbose
	    rename -fitwidth -width width Width
	    rename -fitheight -height height Height
	}
	pack $itk_component(image) -side right -fill y -anchor n

	set image_ [$itk_component(image) get_image]
	set canvas_ [$itk_component(image) get_canvas]
	
	# create a rectangle to mark previos position
	set marker_ [$canvas_ create rectangle 0 0 0 0 \
			 -outline black \
			 -width 2]

	# create the panning rectangle for resizing and positioning
	set panner_ [$canvas_ create rectangle 0 0 0 0 \
			 -outline white \
			 -fill black \
			 -stipple pat7 \
			 -width 1]
	
	# get name of CanvasDraw widget for image
	set draw_ [$itk_component(image) component draw]
	# don't allow resize for now (difficult to implement right)
	$draw_ config -show_selection_grips 0

	# add bindings for moving and resizing the panning rect
	$draw_ add_object_bindings $panner_

	# setup callbacks for moving and resizing image
	$draw_ add_notify_cmd $panner_ [code $this notify_cmd]
	
	# zoom in and out with mouse 2 and 3 (1 is for panning)
	bind $canvas_ <2> "$itk_option(-target_image) inc_zoom 1"
	bind $canvas_ <3> "$itk_option(-target_image) inc_zoom -1"

	# make this image a "view" of the target image
	$target_image_ view add $image_ 0

	# center image on resize
	bind $w_ <Configure> [code $itk_component(image) center]

	add_short_help $w_ $itk_option(-shelp)

	# only start panning after the image is displayed
	after 0 [code $this init_panning]
    }

    
    # Initialize the pan window after a new image has been loaded and
    # arrange to be notified of changes in the position and size of the 
    # visible image.

    public method init_panning {} {
	set panImageWidth_ [$image_ dispwidth]
	set panImageHeight_ [$image_ dispheight]

	if {$panImageWidth_ == 0} {
	    $target_image_ pan start [code $this pan] 1
	    return
	}

	# just center image in canvas window
	$itk_component(image) center

	# max pan factor is -1 (no shrink, -2 is 1/2 size...)
	set panFactor_ [lindex [$image_ scale] 0]
	if {"$panFactor_" == "" || $panFactor_ >= 0} {
	    set panFactor_ -1
	}
	$target_image_ pan start [code $this pan] $panFactor_
	
	# set the valid area to move the panning rect
	$draw_ configure -bbox [$canvas_ bbox $image_]
	
	# draw a compass indicating N and E
	catch draw_compass
    }


    # stop the panning callback

    public method stop_panning {} {
	$target_image_ pan stop
    }


    # update the panner rectangle to display the current position and size
    # of the target image
    # x1 y1 x2 y2 give the visible portion of the image
    # if "changed" is 1, there is a new image with pos. different dimensions.
    # PWD: if changed is true always do this, includes cases when orientation
    # is changed (want to see the compass update).

    protected method pan {x1 y1 x2 y2 changed} {
	set scale [lindex [$image_ scale] 0]
        if { [info exists coords_]  && ! $changed } {
            if { $x1 == $coords_(pan_x1) && $y1 == $coords_(pan_y1) && \
		 $x2 == $coords_(pan_x2) && $y2 == $coords_(pan_y2) && \
		 "$scale" == "$coords_(scale)" && \
                 $panImageHeight_ == $coords_(panImageHeight) && \
		 $panImageWidth_ == $coords_(panImageWidth) &&
                 $panImageWidth_ > 1 } {
                return
            }
        }
        set coords_(pan_x1) $x1
        set coords_(pan_y1) $y1
        set coords_(pan_x2) $x2
        set coords_(pan_y2) $y2
        set coords_(scale)  $scale
        set coords_(panImageWidth) $panImageWidth_
        set coords_(panImageHeight) $panImageHeight_

	if {$changed} {
	    init_panning
	} else {
	    if {$x1 == 0 && $y1 == 0 && abs($x2-$panImageWidth_)<3 && abs($y2-$panImageHeight_)<3} {
		$itk_option(-target_image) center
	    }
	    $canvas_ coords $marker_ [incr x1 -2] [incr y1 -2] [incr x2 2] [incr y2 2]
	    $canvas_ coords $panner_ $x1 $y1 $x2 $y2
	}
    }

    
    # this method is called when the user moves or resizes the panning rect.
    # op is set to "resize" or "move" (resize not currently supported)
    # PWD: changed to deal with very narrow images (i.e. spectra).

    public method notify_cmd {op} {
       if {$panImageWidth_ == 0 && $panImageHeight_ == 0} {
          return
       }
       lassign [$canvas_ coords $panner_] x1 y1 x2 y2
       if {"$op" == "move" } {
          if {$panImageWidth_ > 1} {
             $target_canvas_ xview moveto [expr $x1/($panImageWidth_-1)]
          } else {
             $target_canvas_ xview moveto $x1
          }
          if { $panImageHeight_ > 1 } {
             $target_canvas_ yview moveto [expr $y1/($panImageHeight_-1)]
          } else {
             $target_canvas_ yview moveto $y1
          }
          $target_image_ pan update
          $itk_option(-target_image) maybe_center
       }
       return 0
    }

    # draw an ra,dec compass indicating N and E by following lines along ra and dec
    # near the center of the image

    protected method draw_compass {} {
	$canvas_ delete compass

	# get image size in arcsec
	if {[$image_ isclear] || "[$image_ wcswidth]" == ""} {
	    return
	}

	set wcsw [expr {[$image_ wcswidth]*60}]
	set wcsh [expr {[$image_ wcsheight]*60}]

	# set initial size of compass to percent of the image size in arcsecs
	set size_ [expr {[min $wcsw $wcsh]/4}]
	
	# size in deg
	set size_deg [expr {$size_/3600.}]

	# get image equinox
	set equinox [$image_ wcsequinox]
	set deg_eq  "deg $equinox"

	# start at center of image (in deg)
	lassign [$image_ wcscenter -format 1] ra0 dec0

	# check if at north or south pole, since that is a special case
	if {90-abs($dec0) < $wcsh/3600} {
	    # skip this if at the pole (for now)
	    return
	} 

	# get end points of compass so we can determine the directions
	set ra1 [expr {$ra0+$size_deg/cos(($dec0/180.)*$pi_)}]
	if {$ra1 < 0} {
	    set ra1 [expr {360+$ra1}]
	}

	set dec1 [expr {$dec0+$size_deg}]
	if {$dec1 >= 90} {
	    set dec1 [expr {180-$dec1}]
	}

	# end points in canvas coords
	$image_ convert coords $ra0 $dec0 $deg_eq cx0 cy0 canvas
	$image_ convert coords $ra1 $dec0 $deg_eq cx1 cy1 canvas
	$image_ convert coords $ra0 $dec1 $deg_eq cx2 cy2 canvas
       
        # directions
        set t1 [expr atan2($cy1-$cy0,$cx1-$cx0)]
        set t2 [expr atan2($cy2-$cy0,$cx2-$cx0)]

        # make sure lengths are 0.25 of the width pixels
        set w [expr 0.25*$itk_option(-width)]
        set cx1 [expr $cx0+$w*cos($t1)]
        set cy1 [expr $cy0+$w*sin($t1)]

        set cx2 [expr $cx0+$w*cos($t2)]
        set cy2 [expr $cy0+$w*sin($t2)]

	# East line
	$canvas_ create line $cx0 $cy0 $cx1 $cy1 \
	    -tags {compass objects} \
	    -fill white \
	    -arrow last

	# North line
	$canvas_ create line $cx0 $cy0 $cx2 $cy2 \
	    -tags {compass objects} \
	    -fill white \
	    -arrow last

	# factor for positioning N and E labels
	set f 0.25

	# label "E"
	$canvas_ create text [expr {$cx1+($cx1-$cx0)*$f}] [expr {$cy1+($cy1-$cy0)*$f}] \
	    -text E \
	    -anchor c \
	    -font $compassfont_ \
	    -fill white \
	    -tags {compass objects}

	# label "N"
	$canvas_ create text [expr {$cx2+($cx2-$cx0)*$f}] [expr {$cy2+($cy2-$cy0)*$f}] \
	    -text N \
	    -anchor c \
	    -font $compassfont_ \
	    -fill white \
	    -tags {compass objects}
    }
    
    # -- public vars --
    
    # target RtdImage (itcl widget)
    itk_option define -target_image target_image Target_image {} {
	# save name of target image and canvas window it is in
	set target_image_ [$itk_option(-target_image) get_image]
	set target_canvas_ [$itk_option(-target_image) get_canvas]
    }

    # width of pan frame
    itk_option define -width width Width 150

    # height of pan frame
    itk_option define -height height Height 150

    # flag: if true, pan image is "subsampled" when shrinking, 
    # otherwise the pixels are averaged
    itk_option define -subsample subsample Subsample 1
   
    # X shared memory option
    itk_option define -usexshm useXshm UseXshm 1

    # X synchronisation option
    itk_option define -usexsync useXsync UseXsync 1

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0}

    # default cursor
    itk_option define -cursor cursor Cursor {}

    # zoom window to update
    itk_option define -zoomwin zoomWin ZoomWin {}

    # help text displayed when mouse enters widget
    itk_option define -shelp shelp Shelp {Pan window: \
		      {bitmap dragb1} = position image, \
		      {bitmap b2} = zoom in, \
		      {bitmap b3} = zoom out}

    # -- protected vars --
    
    # internal target image being panned (rtdimage) 
    protected variable target_image_

    # target image's canvas window
    protected variable target_canvas_
 
    # panning image
    protected variable image_

    # canvas for panning image
    protected variable canvas_

    # name of CanvasDraw widget for image
    protected variable draw_

    # canvas id of the panning rectangle
    protected variable panner_

    # canvas id of a second rectangle used to mark old position
    protected variable marker_

    # width of the panning image, after shrinking
    protected variable panImageWidth_

    # height of the panning image, after shrinking
    protected variable panImageHeight_

    # amount panning image was shrunk (=2 = 1/2, -4 = 1/4, ...)
    protected variable panFactor_

    # const PI
    protected variable pi_ 3.14159265358979323846

    # compass label fonts
    protected variable compassfont_ *-Courier-Bold-R-Normal-*-100-*

    # current pan coords
    protected variable coords_    
}

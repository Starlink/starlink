 # E.S.O. - VLT project 
# "@(#) $Id$"
#
# RtdImagePick.tcl - widget to select an object (star)
#
# See man page RtdImagePick(n) for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  25 Jun 96  Created
# Peter W. Draper 18 Jul 97  Modified font to 
#                            -adobe-helvetica-bold-r-normal-*-12*
#                            -adobe-helvetica-medium-r-normal-*-12*
#                            from 
#                            -adobe-helvetica-bold-r-normal--12*
#                            -adobe-helvetica-medium-r-normal--12*
#                            which failed to be found on 100 Linux systems.


itk::usual RtdImagePick {}

# RtdImagePick is an itcl widget to select an object (star) in an 
# image and get statistics for it. It is based on the rtdimage(3) "statistics" 
# subcommand.

class rtd::RtdImagePick {
    inherit util::TopLevelWidget

    #  create a new RtdImagePick widget

    constructor {args} {
	eval itk_initialize $args
 	wm title $w_ "Pick Object"
	make_layout
	update_scale
    }

    
    # destructor

    destructor {
	catch {cancel}
	catch {$target_image_ view remove $image_}
    }

    
    # called after the options have been evaluated

    method init {} {
    }
    
    
    # do the window layout

    method make_layout {} {
	# main controls frame
	pack [set mainf [frame $w_.mainf]] \
	    -side top -fill both -expand 1 -padx 1m -pady 1m -ipadx 1m -ipady 1m

	pack [set topf [frame $mainf.topf]] \
	    -side top -fill both -expand 1

	make_labels $topf
	make_scale $mainf
	make_rect $topf
	add_buttons
    }


    # make the window to display the statistics in the given frame

    method make_labels {w} {
	# frame for labels
	pack [set labelf [frame $w.labelf -bd 2 -relief groove]] \
	    -side left -fill both -expand 0

	# title for label area
	pack [label $labelf.title -text "Image Statistics:"] \
	    -side top -padx 1m -pady 1m
	
	itk_component add x {
	    util::LabelValue $labelf.x \
		-text "Image X:" \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth $itk_option(-labelwidth) \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	} 
	itk_component add y {
	    util::LabelValue $labelf.y \
		-text "Image Y:" \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth $itk_option(-labelwidth) \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	} 
	itk_component add ra {
	    util::LabelValue $labelf.ra \
		-text "a:" \
		-labelfont $itk_option(-wcsfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth $itk_option(-labelwidth) \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	} 
	itk_component add dec {
	    util::LabelValue $labelf.dec \
		-text "d:" \
		-labelfont $itk_option(-wcsfont) \
		-valuefont $itk_option(-valuefont) \
		-labelwidth $itk_option(-labelwidth) \
		-valuewidth $itk_option(-valuewidth) \
		-relief groove \
		-anchor e
	} 
	itk_component add equinox {
	    util::LabelValue $labelf.equinox \
		-text "Equinox:" \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief groove \
		-anchor e
	} 
	itk_component add object {
	    util::LabelValue $labelf.object \
		-text "Peak object level above bg:" \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief groove \
		-anchor e
	} 
	itk_component add background {
	    util::LabelValue $labelf.background \
		-text "Background level:" \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief groove \
		-anchor e
	} 
	itk_component add fwhm {
	    util::LabelValue $labelf.fwhm \
		-text "FWHM X:Y:" \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief groove \
		-anchor e
	} 
	itk_component add angle {
	    util::LabelValue $labelf.angle \
		-text "Angle of X axis:" \
		-labelfont $itk_option(-labelfont) \
		-valuefont $itk_option(-valuefont) \
		-valuewidth $itk_option(-valuewidth) \
		-labelwidth $itk_option(-labelwidth) \
		-relief groove \
		-anchor e
	} 
	pack \
	    $itk_component(x) \
	    $itk_component(y) \
	    $itk_component(ra) \
	    $itk_component(dec) \
	    $itk_component(equinox) \
	    $itk_component(object) \
	    $itk_component(background) \
	    $itk_component(fwhm) \
	    $itk_component(angle) \
	    -side top -padx 0.5m -pady 0.5m

	add_short_help $itk_component(x)  {X image (pixel) coordinate}
	add_short_help $itk_component(y)  {Y Image (pixel) coordinate}
	add_short_help $itk_component(ra)  {World Coordinates RA value}
	add_short_help $itk_component(dec)  {World Coordinates DEC value}
	add_short_help $itk_component(equinox) {World Coordinates equinox (default: J2000)}
	add_short_help $itk_component(object) {Object: peak value of object above background}
	add_short_help $itk_component(background) {Background: mean background level}
	add_short_help $itk_component(fwhm) {FWHM: full width half maximum in X and Y}
	add_short_help $itk_component(angle) {Angle: angle of major axis, degrees, along X = 0}
    }


    # add a scale widget to change the size of the image area to examine

    method make_scale {w} {
	pack [set scalef [frame $w.scalef -bd 2 -relief groove]] \
	    -side top -fill x -expand 1 -padx 1m -pady 1m

	itk_component add scale {
	    scale $scalef.scale \
		-label $scale_label_ \
		-orient horizontal \
		-from 10 \
		-to $itk_option(-maxsize) \
		-showvalue 0 \
		-highlightthickness 0 \
		-command [code $this update_rect]
	} 
	pack $itk_component(scale) -side top -fill x -expand 1
	$itk_component(scale) set $itk_option(-samplesize)
	add_short_help $itk_component(scale) \
	    {Scale: set the size of the image area to examine (in FITS image pixels)}
    }


    # make the window to display the part of the image being examined

    method make_rect {w} {
	# frame for rect
	pack [set rectf [frame $w.rectf -bd 2 -relief groove]] \
	    -side left -fill both -expand 0

	# add a frame to show the size of the square selected by the scale
	pack [label $rectf.label -text "Area of image to be examined:"] \
	    -side top -padx 1m -pady 1m

	pack [set rf [frame $rectf.rf \
			  -bd 2 -relief groove \
			  -width $itk_option(-maxsize) \
			  -height $itk_option(-maxsize)]] \
	    -fill none -expand 1 -anchor c

	# display the section of the image that will be used
	itk_component add image {
	    RtdImage $rf.image \
		-name "PickImage" \
		-verbose $itk_option(-verbose) \
		-scrollbars 0 \
		-drag_scroll 0 \
		-show_object_menu 0 \
		-graphics 0 \
		-canvaswidth $itk_option(-maxsize) \
		-canvasheight $itk_option(-maxsize) \
		-debug $itk_option(-debug) \
		-usexshm $itk_option(-usexshm) \
                -usexsync $itk_option(-usexsync) \
		-shelp {PickImage: displays section of image being examined}
		
	} {
	}
	pack $itk_component(image) -anchor c -fill none -expand 0
	set image_ [$itk_component(image) get_image]
	set canvas_ [$itk_component(image) get_canvas]
	$itk_component(image) config \
	    -canvaswidth $itk_option(-samplesize) \
	    -canvasheight $itk_option(-samplesize)
	catch {$target_image_ view remove $image_}
	$target_image_ view add $image_ 1
    }


    # add a row of dialog buttons at the bottom of the window

    method add_buttons {} {

	# add dialog buttons
	itk_component add buttons {
	    frame $w_.buttons -borderwidth 2 -relief groove
	}
	pack $itk_component(buttons) \
	    -side top -fill x -padx 1m -pady 1m
	
	itk_component add pick {
	    button $w_.pick \
		 -text "Pick Object" \
		 -command [code $this pick_object]
	}
	itk_component add cancel {
	    button $w_.cancel \
		 -text "Cancel" \
		 -command [code $this cancel]
	}
	itk_component add close {
	    button $w_.close \
		 -text "Close" \
		 -command [code $this close]
	}
	pack \
	    $itk_component(pick) \
	    $itk_component(cancel) \
	    $itk_component(close) \
	    -side left -expand 1 -padx 2m -pady 2m -in $itk_component(buttons)

	add_short_help $itk_component(pick) \
	    "Pick Object: {bitmap b1} = select object in image and\
             display center and other statistics"
	add_short_help $itk_component(cancel) {Cancel: cancel the current pick operation}
	add_short_help $itk_component(close) {Close: close the pick window}
    }



    # update the size of the square in the canvas from the scale widget.
    # "size" is the size of the sample image in image pixels.

    method update_rect {size} {
	set sampleSize_ $size

	# note: need to specify $size for both inx and iny, in case image is rotated
	$target_image_ convert dist $size $size canvas {} num_pixels image
	if {$num_pixels < 5} {
	    return
	}
	$itk_component(scale) config -label "$scale_label_ [expr int($num_pixels+0.5)]"
	$itk_component(image) config -canvaswidth $size -canvasheight $size
	$target_image_ view update $image_ $size $size image

    }

    
    # This method is called when the scale of the image is changed to update the
    # scale widget and display to keep the same number of image pixels.
    # 
    # fx and fy are the scale factors for the canvas (the image is already scaled).

    method update_scale {{fx 1} {fy 1}} {
	$canvas_ scale mark 0 0 $fx $fy

	# set lower bound on scale widget
	# Note: Don't allow statistics on area of image less than about 5x5 pixels
	# (bug in midas C routines when width is small ? Problem in sort routine...? )
	$itk_component(scale) config -from [expr $fx*5]

	# update the sample image
	update_rect $sampleSize_
    }

    
    # let the user select a point in the image and get the statistics on
    # the area

    method pick_object {} {
	cancel
	
	# can't work with scale < 1 (shrunken image) - silently reset scale to 1
	if {[lindex [$target_image_ scale] 0] < 1} {
	    $itk_option(-target_image) scale 1 1
	}

	$target_image_ zoomview start $image_ 1 1 2
	update_rect $sampleSize_
	$w_ configure -cursor cross
	$itk_component(pick) config -state disabled
	set_values {}

	# wait for user to click in image
	set list [pick_object_in_image $sampleSize_ $sampleSize_]

	$itk_component(pick) config -state normal
	$w_ configure -cursor {}
	$target_image_ zoomview stop 2

	if {[llength $list] == 10} {
	    set_values $list
	}

	utilRaiseWindow $w_
    }
    
    
    # format a floating point value (which may also be empty)

    method format_val {val} {
	if {"$val" == ""} {
	    return
	}
	return [format {%.1f} $val]
    }
    

    
    # set the values of the labels from the list (results of "pick_object" call).
    # If list is empty the labels are cleared.
    # If the list is not empty, mark the ra,dec spot in the image.

    method set_values {list} {
	lassign $list x y ra dec equinox fwhmX fwhmY angle object background

	$itk_component(x) config -value $x
	$itk_component(y) config -value $y
	$itk_component(ra) config -value $ra
	$itk_component(dec) config -value $dec
	$itk_component(equinox) config -value $equinox

	$itk_component(angle) config -value [format_val $angle]
	$itk_component(object) config -value [format_val $object]
	$itk_component(background) config -value [format_val $background]
	$itk_component(fwhm) config -value "[format_val $fwhmX] : [format_val $fwhmY]"

	if {"$x" != "" && "$y" != ""} {
	    mark_spot $x $y $image_ $canvas_ $angle $fwhmX $fwhmY
	    mark_spot $x $y $target_image_ $target_canvas_ $angle $fwhmX $fwhmY
	}
    }


    # mark the given x,y image coordinate point in the given image/canvas with
    # a cross with the given width, height (image pixels) and angle (deg).

    method mark_spot {imagex imagey image canvas angle width height} {
	# get canvas coords from image coords
	$image convert coords $imagex $imagey image xc yc canvas
	$image convert dist $width $height image w h canvas

	# get bounding box
	set dx [expr $w/2]
	set dy [expr $h/2]

	# rotate by angle
	# convert to radian
	set rad [expr $angle/57.2958]
	set cosa [expr cos($rad)]
	set sina [expr sin($rad)]

	set rx1 [expr $xc+$cosa*$dx]
	set ry1 [expr $yc-$sina*$dx]

	set rx2 [expr $xc-$sina*$dy]
	set ry2 [expr $yc-$cosa*$dy]

	set rx3 [expr $xc-$cosa*$dx]
	set ry3 [expr $yc+$sina*$dx]

	set rx4 [expr $xc+$sina*$dy]
	set ry4 [expr $yc+$cosa*$dy]

	set bg black
	set fg white
	set tags "mark objects"
	$canvas delete $tags
	
	$canvas create line $rx3 $ry3 $rx1 $ry1 \
	    -fill $bg \
	    -width 5 \
	    -tags $tags

	$canvas create line $rx2 $ry2 $rx4 $ry4 \
	    -fill $bg \
	    -width 5 \
	    -tags $tags

	$canvas create line $rx3 $ry3 $rx1 $ry1 \
	    -fill $fg \
	    -width 2 \
	    -tags $tags

	$canvas create line $rx2 $ry2 $rx4 $ry4 \
	    -fill $fg \
	    -width 2 \
	    -tags $tags
   }

    
    # cancel the current pick operation

    method cancel {} {
	$canvas_ delete mark
	$target_canvas_ delete mark
	$target_image_ zoomview stop 2
	cancel_pick
    }


    # close this window
    
    method close {} {
	cancel
	after idle "destroy $w_"
    }

    # this method is called to allow the user to pick an object in the main image.
    # The arguments indicate the size of the area around the mouse ptr to examine.
    # The return value is a list of:
    # "ra dec equinox fwhmX fwhmY angle objectPeak meanBackground" 
    # as returned from the rtdimage "statistics" subcommand,
    # or an error.
    
    method pick_object_in_image {width height} {
	# info_dialog "Please select an object in the image window with button 1" $w_
	set cursor [$target_canvas_ cget -cursor]
	$target_canvas_ configure -cursor cross

	set bindtags [bindtags $target_canvas_]
	set tag pick$w_
	bind $tag <ButtonRelease-1> [code $this picked_object]
	bindtags $target_canvas_ $tag

	global ::$w_.picked
	set $w_.picked {}
	tkwait variable $w_.picked

	bindtags $target_canvas_ $bindtags

	$target_canvas_ configure -cursor $cursor
	set ret [set $w_.picked]
	
	if {"$itk_option(-command)" != ""} {
	    eval $itk_option(-command) [list $ret]
	}

	return $ret
    }

    
    # this method is called when the user clicks in the image to select an object
    # or star for the "pick_object" method. 

    method picked_object {} {
	# display busy cursor in image and pick window...
	$itk_option(-target_image) busy {
	    busy {
		if {[catch {set list [$image_ statistics]} msg]} {
		    error_dialog $msg
		    cancel_pick
		} else {
		    picked_wcs_object $list
		}
	    }
	}
    }


    # this method is called when the user clicks in the image to select an object
    # or star for the "pick_object" method. In this case, the x,y and ra,dec position 
    # are fixed and only the other info should be calculated (used).

    method picked_special_object {x y ra dec equinox} {
	# display busy cursor in image and pick window...
	$itk_option(-target_image) busy {
	    busy {
		if {[catch {set list [$image_ statistics]} msg]} {
		    error_dialog $msg
		    cancel_pick
		} else {
		    lassign $list {} {} {} {} {} fwhmX fwhmY angle object background
		    picked_wcs_object \
			[list $x $y $ra $dec $equinox $fwhmX $fwhmY $angle $object $background]
		}
	    }
	}
    }


    # this method can be called when the user has selected an object 
    # or star for the "pick_object" method. 
    # The argument should be the value returned from the rtdimage
    # "statistics" subcommand

    method picked_wcs_object {retval} {
	global ::$w_.picked
	set $w_.picked $retval
    }

    
    # cancel the wait for the pick_object method and reset the cursor

    method cancel_pick {} {
	global ::$w_.picked
	set $w_.picked ""
    }


    # -- options --

    # target (main) RtdImage itcl widget
    itk_option define -target_image target_image Target_image {} {
	set target_image_ [$itk_option(-target_image) get_image]
	set target_canvas_ [$itk_option(-target_image) get_canvas]
    }

    # X shared memory option
     itk_option define -usexshm useXshm UseXshm 1

    # X synchronisation option
    itk_option define -usexsync useXsync UseXsync 1

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0}

    # font to use for labels
    itk_option define -labelfont labelFont LabelFont -adobe-helvetica-bold-r-normal-*-12*

    # font to use for values
    itk_option define -valuefont valueFont ValueFont -adobe-helvetica-medium-r-normal-*-12*

    # font to use for ra,dec labels (alpha, delta)
    itk_option define -wcsfont wcsFont WcsFont -*-symbol-*-*-*-*-14-*-*-*-*-*-*-*

    # set the width for  displaying labels and values
    itk_option define -labelwidth labelWidth LabelWidth 22

    # set the width for  displaying labels and values
    itk_option define -valuewidth valueWidth ValueWidth 13

    # set the default size of the image sample area (image dist)
    itk_option define -samplesize sampleSize SampleSize 50

    # set the max size of the image sample area (screen dist)
    itk_option define -maxsize maxSize MaxSize 150

    # command to evaluate when a selection is made or canceled
    itk_option define -command command Command {}

    # debugging flag
    itk_option define -debug debug Debug 0

    # -- protected vars -- 
    
    # internal target image
    protected variable target_image_

    # internal target canvas
    protected variable target_canvas_
    
    # internal canvas widget
    protected variable canvas_

    # internal rtd image
    protected variable image_

    # current sample size
    protected variable sampleSize_ 50

    # label for scale widget
    protected variable scale_label_ "Sample size (in image pixels): "
}







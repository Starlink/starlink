# E.S.O. - VLT project 
# "@(#) $Id: RtdImage.tcl,v 1.55 1999/03/19 20:09:38 abrighto Exp $"
#
# RtdImage.tcl - itcl widget wrapper for the rtdimage type extension
#
# This widget is based on the Tk rtdimage extension, which is implemented 
# in C++.  It simplifies things by creating its own frame, canvas and 
# scrollbars.
#
# See man page RtdImage(n) for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01/06/98  Created
# P.Biereichel    05/08/96  Added itk_option(-with_warp)
# Allan Brighton  22/03/98  Added focus_ method to fix focus problems
#                           and conflicts between menu traversal and
#                           mouse cursor warp code (from Peter Draper,
#                           Starlink).

itk::usual RtdImage {}

# The RtdImage widget is an [incr Tk] interface to the rtdimage extended
# Tk image type. The widget creates a canvas window with optional
# scrollbars and a canvas image item to hold the image. An optional
# canvas line graphics editor is also created by default, to manage
# drawing on the image.  The RtdImage widget can be treated pretty much
# like any standard Tk widget and can be inserted in a Tk frame with the
# pack(n) command. Applications using the RtdImage widget, can access
# the underlying image object and the canvas window to overlay graphics
# on the image.
#
# In addition to the methods below, this class also forwards methods
# implemented in the C++ rtdimage code. It is, however, usually more
# efficient to use the "get_image" method to get a handle for the
# internal rtdimage object and use it directly.

itcl::class rtd::RtdImage {
    inherit util::FrameWidget

    #  create a new RtdImage widget

    constructor {args} {
	global ::rtd_library

	# frame to hold image and scrollbars
	itk_component add imagef {
	    frame $w_.imagef -background black
	} {
	    keep -borderwidth -relief
	}
	pack $itk_component(imagef) -fill both -expand 1 

	# vertical scrollbar frame
	itk_component add vscrollf {
	    frame $itk_component(imagef).vscrollf -background black
	}
	pack $itk_component(vscrollf) -side right -fill y

	# horizontal scrollbar frame
	itk_component add hscrollf {
	    frame $itk_component(imagef).hscrollf -background black
	}
	pack $itk_component(hscrollf) -side bottom -fill x

	# canvas
	# note: create canvas in global namespace, to avoid problems in C++ side
	set cmd \
	    [list canvas $itk_component(imagef).canvas \
		 -xscrollincr 1 \
		 -yscrollincr 1 \
		 -background black \
		 -insertofftime 0]
	
	# Tk canvas containing the image
	itk_component add canvas {
	    set canvas_ [uplevel "#0" $cmd]
	} {
	    rename -relief -canvasrelief canvasRelief CanvasRelief
	    rename -borderwidth -canvasborderwidth canvasBorderwidth CanvasBorderwidth
	    rename -background -canvasbackground canvasBackground CanvasBackground
	    rename -width -canvaswidth canvasWidth CanvasWidth
	    rename -height -canvasheight canvasHeight CanvasHeight
	}
	pack $canvas_ -fill both -expand 1

	# create the image 
	# note: create (for now) in global namespace to avoid problems in C++ interface
	set cmd [list image create rtdimage \
		     -newimagecmd [code $this new_image_cmd]]
	set image_ [uplevel "#0" $cmd]
	
	# put the image in the canvas
	set imageId_ [$canvas_ create image 0 0 \
			  -image $image_ \
			  -anchor nw \
			  -tags $image_]

	bind $canvas_ <Configure> [code $this maybe_center]
	
	eval itk_initialize $args
    }

    
    # destructor - clean up when deleted
    
    destructor {
	catch {$itk_component(draw) deselect_objects}
	catch {$canvas_ delete $image_}
	catch {image delete $image_}
    }
    
    
    # this method is called from the base class (TopLevelWidget) after all
    # the options have been evaluated

    protected method init {} {
	if {$itk_option(-with_warp)} {
	    # make arrow keys move mouse pointer by one pixel
	    bind $canvas_ <Left> "+$image_ warp -1 0"
	    bind $canvas_ <Right> "+$image_ warp 1 0"
	    bind $canvas_ <Up> "+$image_ warp 0 -1"
	    bind $canvas_ <Down> "+$image_ warp 0 1"
	    global ::$w_.focus
	    set $w_.focus {}
	    bind $canvas_ <Enter> "+[code $this focus_ in]" 
	    bind $canvas_ <Leave> "+[code $this focus_ out]" 
	}
    }

   # Control the focussing of the canvas. Only take focus if the
   # top-level window associated with this canvas has the focus
   # (i.e. it's not in another toplevel somewhere). If this isn't
   # done then mysterious raises of the main image window can occur
   # with some window managers (mainly CDE, with click-to-focus).
   #
   # allan: 19.6.98: disabled the above behavior, since it causes
   # problems with mouse warping and confuses people. Can't verify
   # the CDE behavior...

   protected method focus_ {way} {
      global ::$w_.focus
      set top [winfo toplevel $w_]
      set focus [focus -displayof $top]
      if { $focus != {} } {
         #if {[winfo toplevel $focus] == "$top" } { 
            
            #  This toplevel has the focus (or at least a child of it
            #  has), so it's ok to proceed.
            if { $way == "in" } { 
               set $w_.focus [focus -displayof .]
               catch {focus $canvas_}
            } else {
               catch {focus [set $w_.focus]}
            }
         #}
      }
   }


    # utility to update an option in the image
    # Note: this works automatically with "widgets", but itk doesn't work
    # with "images"...
    
    protected method imageconfig_ {option} {
	$image_ config $option $itk_option($option)
    }


    # return the name of the underlying rtdimage object
    
    public method get_image {} {
	return $image_
    }

    
    # return the name of the underlying canvas widget
    
    public method get_canvas {} {
	return $canvas_
    }


    # return the canvas Id for the image
    
    public method get_imageId {} {
	return $imageId_
    }


    # update the allowed interactive drawing area in the canvas

    protected method set_drawing_area {} {
	if {[info exists itk_component(draw)] && ! [$image_ isclear]} {
	    $image_ convert coords 1 1 image x0 y0 canvas
	    $image_ convert coords \
		[expr [$image_ width]-1] \
		[expr [$image_ height]-1] \
		image x1 y1 canvas
	    set cx0 [expr int([min $x0 $x1])]
	    set cy0 [expr int([min $y0 $y1])]
	    set cx1 [expr int([max $x0 $x1])]
	    set cy1 [expr int([max $y0 $y1])]
	    $itk_component(draw) configure -bbox "$cx0 $cy0 $cx1 $cy1"
	}
    }

    
    # make the graphics toolbox and menu

    protected method make_toolbox {} {
	# CanvasDraw(n) object, used to manage the canvas graphics
	itk_component add draw {
	    util::CanvasDraw $w_.draw \
		-canvas $canvas_ \
		-transient 1 \
		-withdraw 1 \
		-center 0 \
		-shorthelpwin $itk_option(-shorthelpwin) \
		-withtoolbox $itk_option(-withtoolbox) \
		-defaultcursor $itk_option(-cursor) \
		-show_object_menu $itk_option(-show_object_menu) \
		-regioncommand $itk_option(-regioncommand)
	}

	set_drawing_area

	# clicking on the image or image background deselects other objects
	$canvas_ bind $image_ <1> [code $itk_component(draw) deselect_objects]
    }


    # display the toolbox window
    
    public method show_toolbox {} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	if {! [info exists itk_component(draw)]} {
	    # user must have deleted window...
	    make_toolbox
	} else {
	    # $itk_component(draw) center_window
	    wm deiconify $itk_component(draw)
	    wm transient $itk_component(draw) $w_
	}
    }


    # resize the image and the canvas graphics by the given integer factors
    # (1 is no scale, -2 = 50%, 2 = 200% etc...)
    # - deselect canvas graphics (so handles don't get scaled)
    
    public method scale {x y} {

	# don't resize selection grips
	if {$itk_option(-graphics)} {
	    $itk_component(draw) deselect_objects
	}

	save_scroll_pos_

	# note previous scale and position
	lassign [$image_ scale] xs ys
	if {"$xs" == ""} {
	    set xs [set ys 1]
	} else {
	    # scale the image
	    if {[catch {$image_ scale $x $y} msg]} {
		error_dialog $msg $w_
		return
	    }
	    # if we passed the max scale factor, it will be ignored
	    if {"[$image_ scale]" == "$xs $ys"} {
		return
	    }
	}

	# scale the canvas items (need relative floating point factor)
	if {$xs < 0} {
	    set xs [expr -1.0/$xs]
	    set ys [expr -1.0/$ys]
	} 
	if {$x < 0} {
	    set x [expr -1.0/$x]
	    set y [expr -1.0/$y]
	}
	# carefull in case scale factor is zero
	set fx  [expr double($x)/$xs]
	set fy [expr double($y)/$ys]
	$canvas_ scale all 0 0 $fx $fy

	# set new scrollregion to include all of image
	set w [$image_ dispwidth]
	set h [$image_ dispheight]
	set_scrollregion 0 0 $w $h

	# notify rapid frame to change size if necessary
	if {[winfo exists $w_.rapid]} {
	    $w_.rapid notify_cmd scale
	} 
	restore_scroll_pos_
	maybe_center

	# update interactive drawing area
	set_drawing_area
	
	# update pick window, if needed
	if {[winfo exists $w_.pick]} {
	    $w_.pick update_scale $fx $fy
	}
    }

    
    # save the current scrolling positions

    protected method save_scroll_pos_ {} {
	lassign [$canvas_ xview] xScroll0_ xScroll1_
	lassign [$canvas_ yview] yScroll0_ yScroll1_
    }


    # restore the relative scrolling positions

    protected method restore_scroll_pos_ {} {
	$canvas_ xview moveto $xScroll0_
	$canvas_ yview moveto $yScroll0_
	lassign [$canvas_ xview] x0 x1
	lassign [$canvas_ yview] y0 y1
	$canvas_ xview moveto [expr $x0-($x1-$xScroll1_)/2.0]
	$canvas_ yview moveto [expr $y0-($y1-$yScroll1_)/2.0]
    }


    # toggle rotation of the image and canvas items

    public method rotate {bool} {
	if {$bool != [$image_ rotate]} {
	    $image_ rotate $bool
	    if {[info exists itk_component(draw)]} {
		$itk_component(draw) rotate all
	    }
	    if {[$image_ dispwidth] != [$image_ dispheight]} {
		center
	    }

	    # notify rapid frame to move if necessary
	    if {[winfo exists $w_.rapid]} {
		$w_.rapid notify_cmd rotate $bool
	    } 

	    # update interactive drawing area
	    set_drawing_area
	}
    }


    # flip or unflip the image and canvas items about the 
    # x or y axis, as given by $xy

    public method flip {xy bool} {
	if {$bool != [$image_ flip $xy]} {
	    set coords [$canvas_ coords $image_]
	    $image_ flip $xy $bool
	    if {[info exists itk_component(draw)]} {
		if {"$xy" == "x"} {
		    $itk_component(draw) flipx all [expr [$image_ dispwidth]-1]
		} else {
		    $itk_component(draw) flipy all [expr [$image_ dispheight]-1]
		}
	    }
	    eval "$canvas_ coords $image_ $coords"

	    # notify rapid frame to move if necessary
	    if {[winfo exists $w_.rapid]} {
		$w_.rapid notify_cmd flip $xy $bool
	    } 

	    # update interactive drawing area
	    set_drawing_area
	}
    }


    # if the image is smaller than the canvas window, center it 
    
    public method maybe_center {} {
	set cw [winfo width $canvas_]
	set ch [winfo height $canvas_]
	set dw [$image_ dispwidth]
	set dh [$image_ dispheight]
	if {$cw != 1} {
	    if {$dw < $cw && $dw} {
		set x [expr (($dw-$cw)/2.0)/$dw]
		$canvas_ xview moveto $x
	    }
	    if {$dh < $ch && $dh} {
		set y [expr (($dh-$ch)/2.0)/$dh]
		$canvas_ yview moveto $y
	    }
	} else {
	    update
	    maybe_center
	    return
	}
	set_scrollregion 0 0 $dw $dh
    }

    
    # set the canvas scrollregion 

    protected method set_scrollregion {x0 y0 x1 y1} {
	$canvas_ config -scrollregion "$x0 $y0 $x1 $y1"
    }

    
    # center the image in the canvas window

    public method center {} {
	set dw [$image_ dispwidth]
	set dh [$image_ dispheight]
	set_scrollregion 0 0 $dw $dh
	set cw [winfo width $canvas_]
	set ch [winfo height $canvas_]
	if {$cw != 1 && $dw && $dh} {
	    $canvas_ xview moveto [expr (($dw-$cw)/2.0)/$dw]
	    $canvas_ yview moveto [expr (($dh-$ch)/2.0)/$dh]
	} 
    }


    # arrange to interactively create a rapid frame to display 
    # a section of the image.
    # If popup is 1, the frame is displayed in a popup window,
    # otherwise at the selected position in the canvas
    
    public method rapid_frame {popup} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}

	if {[winfo exists $w_.rapid]} {
	    destroy $w_.rapid
	} 

	if {[action_dialog \
		 "Please select and drag out a region of the image with mouse button 1" \
		 $w_]} { 
	    $itk_component(draw) set_drawing_mode region [code $this make_rapid_frame $popup]
	}
    }

    
    # delete the rapid frame

    public method delete_rapid_frame {} {
	if {[winfo exists $w_.rapid]} {
	    destroy $w_.rapid
	} 
    }


    # Create a rapid frame to display a section of the image.
    # If popup is 1, the frame is displayed in a popup window,
    # otherwise at the selected position in the canvas
    # "region_id" is the canvas id of the object used to position 
    # and resize the image.
    
    protected method make_rapid_frame {popup region_id x0 y0 x1 y1} {
	set xoffset [expr int($x0)]
	set yoffset [expr int($y0)]
	set width [expr int($x1-$x0+1)]
	set height [expr int($y1-$y0+1)]

	if {$popup} {
	    rtd::RtdImagePopup $w_.rapid \
		-target_image $this \
		-xoffset $xoffset \
		-yoffset $yoffset \
		-width $width \
		-height $height \
		-zoomwin $itk_option(-zoomwin) \
		-subsample $itk_option(-subsample) \
		-usexshm $itk_option(-usexshm) \
                -usexsync $itk_option(-usexsync) \
		-withdraw [expr !$popup] \
		-region_id $region_id \
		-verbose $itk_option(-verbose) \
		-shorthelpwin $itk_option(-shorthelpwin) \
		-transient 1 \
		-min_scale $itk_option(-min_scale) \
		-max_scale $itk_option(-max_scale) \
		-command $itk_option(-rapid_frame_command) \
	} else {
	    rtd::RtdImageFrame $w_.rapid \
		-target_image $this \
		-xoffset $xoffset \
		-yoffset $yoffset \
		-width $width \
		-height $height \
		-subsample $itk_option(-subsample) \
		-usexshm $itk_option(-usexshm) \
                -usexsync $itk_option(-usexsync) \
		-region_id $region_id \
		-verbose $itk_option(-verbose) \
		-command $itk_option(-rapid_frame_command)
	}
    }
    

    # attach the named camera. 
    
    public method attach_camera {camera} {
	# these commands are evaluated before/after real-time events
	set preCmd {}
	set postCmd [code $this camera_post_command]

	if {[catch {$image_ camera attach $camera $preCmd $postCmd} msg]} {
	    error_dialog $msg
	}
	update idletasks
    }


    # stop the camera.
    # note: race conditions might cause display to lag behind the socket data.
    # force an update here.
    
    public method detach_camera {} {
	$image_ camera detach
	$image_ update
    }

    
    # This method is called when a new image has been received from 
    # the camera and before it is displayed.
    # The frameid will be 0 for the main image and non-zero for a rapid frame.

    protected method camera_pre_command {frameid} {
    }

    
    # This method is called whenever a new image has been received from 
    # the camera and displayed.
    # Update the widgets that need to display new values
    # The frameid will be 0 for the main image and non-zero for a rapid frame.

    protected method camera_post_command {frameid} {
	if {$frameid == 0} {
	    if {[winfo exists $w_.spectrum]} {
		$w_.spectrum notify_cmd
	    } 

	    if {"$preview_var_" != ""} {
		global ::$preview_var_
		if {[info exists $preview_var_]} {
		    set $preview_var_ 0
		}
	    }
	    
	    # set up world coordinate info, if needed
	    set_rtd_wcs_info $frameid
	}
    }


    # Set up world coordinate info for an image received from the rtdServer.

    public method set_rtd_wcs_info {frameid} {
    }
    

    # popup a window to display a table of nrows x ncols pixel values 
    # from the image

    public method pixel_table {nrows ncols} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	if {[winfo exists $w_.pixtable]} {
	    destroy $w_.pixtable
	}
	rtd::RtdImagePixTable $w_.pixtable \
	    -image $this \
	    -nrows $nrows \
	    -ncols $ncols \
	    -shorthelpwin $itk_option(-shorthelpwin) \
	    -transient 1
    }


    # arrange to interactively create a spectrum line to display 
    # a graph of the image values along a given line.
    
    public method spectrum {} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}

	if {[winfo exists $w_.spectrum]} {
	    $w_.spectrum quit
	}
	
	if {[action_dialog \
		 "Press OK and then drag out a line over the image with button 1" \
		 $w_]} {
	    $itk_component(draw) set_drawing_mode line [code $this make_spectrum]
	}
    }


    # display a dialog for selecting objects in the image and displaying information
    # about the selected area of the image

    public method pick_dialog {{command ""}} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}

	utilReUseWidget rtd::RtdImagePick $w_.pick \
	    -target_image $this \
	    -command $command \
	    -verbose $itk_option(-verbose) \
	    -orient $itk_option(-pickobjectorient) \
	    -debug $itk_option(-debug) \
	    -shorthelpwin $itk_option(-shorthelpwin)
	$w_.pick pick_object
    }

    
    # this method can be used in bindings to cause a selection in the
    # image (to pick an object/star) to return the given position rather than the
    # calculated center pos. If the optional args are not specified, they are
    # calculated.

    protected method picked_wcs_object {x y ra dec {equinox J2000} {fwhmX ""} {fwhmY ""} \
				  {angle ""} {object ""} {background ""}} {
	if {[winfo exists $w_.pick]} {
	    if {"$angle" == ""} {
		$w_.pick picked_special_object $x $y $ra $dec $equinox
	    } else {
		$w_.pick  picked_wcs_object \
		    [list $x $y $ra $dec $equinox $fwhmX $fwhmY $angle $object $background]
	    }
	}
    }

    
    # make a hard copy of the image display

    public method print {} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	set object [$image_ object]
	set file [file tail $itk_option(-file)]
	set center [$image_ wcscenter]
	set user [id user]
	set app [lindex [winfo name .] 0]
	set date [clock format [clock seconds] -format {%b %d, %Y at %H:%M:%S}]
	
	utilReUseWidget RtdImagePrint $w_.print \
	    -image $this \
	    -show_footer 1 \
	    -whole_canvas 0 \
	    -transient 1 \
	    -top_left "ESO\n$object" \
	    -top_right "$file\n$center" \
	    -bot_left "$user/$app" \
	    -bot_right "$date"
    }


    # Save the current image or a section of the current image to a file in 
    # FITS format chosen from a file name dialog. If dir and pattern are specified,
    # they are used as defaults for the file selection dialog.
    # If x0, y0, x1 and y1 are specified (canvas coordinates), then a section 
    # of the image is saved.
    #
    # The return value is the name of the new file, if any, or an empty string.
    
    public method save_as {{dir "."} {pattern "*"} {x0 ""} {y0 ""} {x1 ""} {y1 ""}} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	set file [filename_dialog $dir $pattern $w_]
	if {"$file" != ""} {
	    if {[file isfile $file]} {
		if {![confirm_dialog \
			  "[file tail $file] exists - Do you want to overwrite it ?" $w_]} {
		    return
		}
		if {[file isdir $file]} {
		    error_dialog "$file is a directory" $w_
		    return
		}
	    }
	    if {"$x0" == ""} {
		$image_ dump $file
	    } else {
		if {[catch {
		    $image_ convert coords $x0 $y0 canvas x0 y0 image
		    $image_ convert coords $x1 $y1 canvas x1 y1 image
		    $image_ dump $file $x0 $y0 $x1 $y1
		} msg]} {
		    error_dialog $msg
		    return
		}
	    }
	    return $file
	}
    }


    # save a section of the current image to a file in FITS format 
    # chosen from a file name dialog.
    
    public method save_region_as {} {
	# check if we have an image
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}

	# can't convert DSS plate cooeficients correctly
	if {"[$image_ fits get PLTRAH]" != ""} {
	    warning_dialog "Can't convert DSS plate coefficients. \
                            Please get the image from the DSS image server." $w_
	    return
	}

	# first get the region to save
	if {[action_dialog \
		 "Please select and drag out a region of the image with mouse button 1" \
		 $w_]} { 
	    $itk_component(draw) set_drawing_mode region [code $this save_region]
	}
    }


    # save the given section of the current image to a file in FITS format 
    # chosen from a file name dialog. The canvas_id is the id if the canvas
    # object used to select the region . The canvas coordinates of the region 
    # are also passed as arguments.
    
    protected method save_region {canvas_id x0 y0 x1 y1} {
	save_as . * $x0 $y0 $x1 $y1
	$itk_component(draw) delete_object $canvas_id
    }


    # Create a graph to display the image data values along the line
    # just created.
    # "line_id" is the canvas id of the line.
    
    protected method make_spectrum {line_id x0 y0 x1 y1} {
	if {[winfo exists $w_.spectrum]} {
	    $w_.spectrum quit
	}
	rtd::RtdImageSpectrum $w_.spectrum \
		-x0 [expr int($x0)] \
		-y0 [expr int($y0)] \
		-x1 [expr int($x1)] \
		-y1 [expr int($y1)] \
		-image $this \
		-transient 1 \
		-shorthelpwin $itk_option(-shorthelpwin) \
		-line_id $line_id
    }

    
    # toggle the visibility of the line graphics
    # (The trace variable name is passed here, if 1, hide the graphics...)

    public method hide_graphics {variable} {
	global ::$variable
	if {[set $variable]} {
	    $canvas_ raise $image_ 
	} else {
	    $canvas_ lower $image_ 
	}
    }


    # this method is called by the image code whenever a new image is loaded
    # (for updates, see camera command)

    protected method new_image_cmd {} {
	# only runs the first time, if the user chose a different color scale
	if {"$itk_option(-color_scale)" != "linear"} {
	    if {[catch {$image_ colorscale $itk_option(-color_scale)} msg]} {
		error_dialog $msg
	    }
	    set itk_option(-color_scale) linear
	}

	if {[winfo exists $w_.rapid]} {
	    destroy $w_.rapid
	} 
	if {[info exists itk_component(draw)]} {
	    $itk_component(draw) clear
	    set_drawing_area
	} 
	if {"$itk_option(-newimagecmd)" != ""} {
	    eval $itk_option(-newimagecmd)
	}
	center
    }

    
    # set preview mode on or off in the image. In this case, the
    # arg is the "name" of a global variable controlling the preview
    # mode. It will be kept up to date by this class.
    
    public method preview {var} {
	global ::$var
	$image_ preview [set [set preview_var_ $var]]
    }


    # set the performance test mode on or off.
    public method perftest {} {
        if {[catch {$image_ perftest on} msg]} {
	    error_dialog $msg
	    return
	}
        utilReUseWidget RtdImagePerf $w_.perf \
            -target_image $this \
            -shorthelpwin $itk_option(-shorthelpwin)
    }

    # Methods for the playing and recording of images.

    public method record {camera} {
        utilReUseWidget RtdRecorderTool $w_.rec \
            -target_image $this \
            -server_camera $camera \
            -shorthelpwin $itk_option(-shorthelpwin)
    }

    
    # clear the current image display and remove an windows that
    # access it

    public method clear {} {
	$image_ config -file ""
	set itk_option(-file) ""
	set w [$image_ dispwidth]
	set h [$image_ dispheight]
	set_scrollregion 0 0 $w $h
	if {[info exists itk_component(draw)]} {
	    $itk_component(draw) clear
	} 
	$canvas_ delete objects
	if {[winfo exists $w_.rapid]} {
	    destroy $w_.rapid
	} 
	if {[winfo exists $w_.spectrum]} {
	    destroy $w_.spectrum
	} 
	if {[winfo exists $w_.pixtable]} {
	    destroy $w_.pixtable
	} 
    }

    
    # reload the image file, if there is one

    public method reopen {} {
	$image_ update
    }


    # load a FITS file (internal version: use -file option/public variable)
    
    protected method load_fits_ {} {
	if {[file exists $itk_option(-file)] || "$itk_option(-file)" == "-"} {
	    if {[catch {$image_ config -file $itk_option(-file)} msg]} {
		error_dialog $msg $w_
		clear
	    }
	    set w [$image_ dispwidth]
	    set h [$image_ dispheight]
	    set_scrollregion 0 0 $w $h
	} else {
	    error_dialog "'$itk_option(-file)' does not exist" $w_
	    set file ""
	    clear
	}
    }


    # pass these methods on to the image widget unchanged
    # (this just generates methods on the fly...)

    ::foreach i {view cut cmap itt colorscale alloccolors zoom zoomview object convert
		 wcscenter wcsradius isclear dispwidth dispheight freq} {
	method $i {args} [::format {return [eval "$image_ %s $args"]} $i]
    }


    # -- public vars --

    # fits image file to display
    itk_option define -file file File {} {
	if {"$itk_option(-file)" != ""} {
	    # this code makes it easier to center the image on startup
	    if {[winfo width $w_] <= 1} {
		after 0 [code $this load_fits_]
	    } else {
		load_fits_
	    }
	}
    }

    # for compatibility with saoimage
    itk_option define -fits fits Fits {} {
	if {"$itk_option(-fits)" != ""} {
	    config -file $itk_option(-fits)
	}
    }
    
    # set displaymode flag 0 to optimize for smooth scrolling, 
    # 1 for faster updates and less memory (works best for main image)
    itk_option define -displaymode displayMode DisplayMode {1} {
	imageconfig_ -displaymode
    }
   
    # X shared memory option
    itk_option define -usexshm useXshm UseXshm 1 {
	imageconfig_ -usexshm
    }

    # X synchronisation option
    itk_option define -usexsync useXsync UseXsync 1 {
        imageconfig_ -usexsync
    }

    # -name option
    itk_option define -name name Name {MainImage} {
	imageconfig_ -name
    }

    # minimum allowed scale value
    itk_option define -min_scale min_scale Min_scale -10

    # maximum allowed scale value
    itk_option define -max_scale max_scale Max_scale 20

    # This flag controls whether the FITS image header is kept in 
    # SysV shared memory (see the rtdRemote interface for use of this)
    itk_option define -shm_header shm_header Shm_header 0 {
	imageconfig_ -shm_header
    }

    # This flag controls whether the FITS image data is kept in 
    # SysV shared memory (see the rtdRemote interface for use of this)
    itk_option define -shm_data shm_data Shm_data 0 {
	imageconfig_ -shm_data
    }

    # Specify the min number of colors to allocate before using
    # a private colormap. Note: this option is currently ignored.
    itk_option define -min_colors min_colors Min_colors 30 {
	imageconfig_ -min_colors
    }

    # Specify the max number of colors to allocate before using
    # a private colormap. Note: this option is currently ignored.
    itk_option define -max_colors max_colors Max_colors 60 {
	imageconfig_ -max_colors
    }

    # if non-zero, shrink image to fit width
    itk_option define -fitwidth fitWidth FitWidth {0} {
	if {$itk_option(-fitwidth)} {
	    config -canvaswidth $itk_option(-fitwidth)
	    imageconfig_ -fitwidth 
	}
    }

    # if non-zero, shrink image to fit height
    itk_option define -fitheight fitHeight FitHeight {0} {
	if {$itk_option(-fitheight)} {
	    config -canvasheight $itk_option(-fitheight)
	    imageconfig_ -fitheight 
	}
    }

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0} {
	imageconfig_ -verbose
    }

    # flag: if true, use quick and dirty algorithm to shrink images
    itk_option define -subsample subsample Subsample {1} {
 	imageconfig_ -subsample
    }

    # flag: if true, display horizontal and vertical scrollbars
    itk_option define -scrollbars scrollbars Scrollbars 0 {
	if {$itk_option(-scrollbars)} {
	    # optional vertical scrollbar
	    if {![info exists itk_component(vscroll)]} {
		itk_component add vscroll {
		    scrollbar $itk_component(vscrollf).vscroll \
			-relief sunken \
			-command [code $canvas_ yview]
		}
		pack $itk_component(vscroll) -side right -fill y
		$canvas_ config -yscrollcommand "$itk_component(vscroll) set"
	    }
	    if {![info exists itk_component(hscroll)]} {
		# optional horizontal scrollbar
		itk_component add hscroll {
		    scrollbar $itk_component(hscrollf).hscroll \
			-relief sunken \
			-orient horiz \
			-command [code $canvas_ xview]
		}
		pack $itk_component(hscroll) -side bottom -fill x
		$canvas_ config -xscrollcommand "$itk_component(hscroll) set"
	    }
	} else {
	    $canvas_ config -xscrollcommand "" -yscrollcommand ""
	    if {[info exists itk_component(vscroll)]} {
		destroy $itk_component(vscroll)
		destroy $itk_component(hscroll)
		unset itk_component(vscroll)
		unset itk_component(hscroll)
	    }
	}
    }

    # flag: if true, set bindings to scroll with the middle mouse button
    itk_option define -drag_scroll drag_scroll Drag_scroll 0 {
	if {$itk_option(-drag_scroll)} {
	    bind $canvas_ <2> [code $canvas_ scan mark %x %y]
	    bind $canvas_ <B2-Motion> [code "$canvas_ scan dragto %x %y; $this maybe_center"]
	} else {
	    bind $canvas_ <2> { }
	    bind $canvas_ <B2-Motion> { }
	}
    }

    # flag: if true, create a CanvasDraw object to manage the canvas graphics
    itk_option define -graphics graphics Graphics 1 {
	if {$itk_option(-graphics) && ![info exists itk_component(draw)]} {
	    # create an object to manage the canvas graphics
	    make_toolbox
	} else {
	    $canvas_ config -cursor $itk_option(-cursor)
	}
    }
    
    # if true (default) create the GUI interface (toolbox), otherwise don't
    itk_option define -withtoolbox withToolbox WithToolbox {1}

    # default cursor
    itk_option define -cursor cursor Cursor {target}

    # Tcl command to evaluate whenever a "region" of the image is selected
    # via the graphic toolbox "region" selection item. Can be used to
    # select graphic items or a section of the image for an operation.
    itk_option define -regioncommand regionCommand RegionCommand {}

    # optional tcl command to be evaluated when a rapid frame is created, moved,
    # resized or deleted: 6 args will be appended: 
    #  
    #  name = unique name for the frame
    #  op  = {move,resize or delete},
    #  x, y = coords of upper left corner of frame in image 
    #  width, height = dimensions of frame.
    itk_option define -rapid_frame_command rapid_frame_command Rapid_frame_command {}

    # default cmap file
    itk_option define -default_cmap default_cmap Default_cmap {real} 

    # default ITT file
    itk_option define -default_itt default_itt Default_itt {ramp}

    # Set the default color scale algorithm to one of: {linear log sqrt histeq}
    itk_option define -color_scale color_scale Color_scale linear

    # Colormap initialization and directory for colormap and ITT files
    itk_option define -cmap_dir cmap_dir Cmap_dir {} {
	if {!$colormap_initialized_} {
	    global ::rtd_library
	    if {"$itk_option(-cmap_dir)" == ""} {
		set itk_option(-cmap_dir) $rtd_library/colormaps
	    }
	    $image_ cmap file \
		$itk_option(-cmap_dir)/$itk_option(-default_cmap).$itk_option(-cmap_suffix)
	    $image_ itt file \
		$itk_option(-cmap_dir)/$itk_option(-default_itt).$itk_option(-itt_suffix)
	    set colormap_initialized_ 1
	}
   }

    # suffix for colormap files
    itk_option define -cmap_suffix cmap_suffix Cmap_suffix {lasc}

    # suffix for ITT files
    itk_option define -itt_suffix itt_suffix Itt_suffix {iasc}

    # flag: if true, display menus over graphic objects when selected with <3>
    itk_option define -show_object_menu show_object_menu Show_object_menu 0

    # name of zoom window to update when mouse enters this window
    itk_option define -zoomwin zoomWin ZoomWin {} {
	if {"$itk_option(-zoomwin)" != ""} {
	    bind $canvas_ <Any-Enter> "+$itk_option(-zoomwin) enter_image $image_"
	    bind $canvas_ <Any-Leave> "+$itk_option(-zoomwin) leave_image $image_"
	}
    }

    # short help text
    itk_option define -shelp shelp Shelp "image window" {
	if {"$itk_option(-shelp)" != ""} {
	    add_short_help $w_ $itk_option(-shelp)
	    add_short_help $canvas_ $itk_option(-shelp)
	}
    }

    # -orient option for Pick Object window
    itk_option define -pickobjectorient pickObjectOrient PickObjectOrient {vertical}

    # command to eval when a new image is loaded
    itk_option define -newimagecmd newImageCmd NewImageCmd  ""
        
    # optionally specify TopLevelWidget to display short help messages
    itk_option define -shorthelpwin shortHelpWin ShortHelpWin {}

    # debugging flag
    itk_option define -debug debug Debug 0

    # option to warp the mouse pointer
    itk_option define -with_warp with_warp With_warp 0

    # -- protected vars --

    # internal rtd image 
    protected variable image_

    # canvas widget
    protected variable canvas_

    # canvas Id for image
    protected variable imageId_
   
    # name of a global variable controlling preview mode
    protected variable preview_var_ {}

    # name of a global variable controlling performance test mode
    protected variable perftest_var_ {}

    # saved x0 relative scrolling position
    protected variable xScroll0_ 0

    # saved x1 relative scrolling position
    protected variable xScroll1_ 0

    # saved y0 relative scrolling position
    protected variable yScroll0_ 0

    # saved y1 relative scrolling position
    protected variable yScroll1_ 0

    # --- common to all instances --
    
    # flag: true if the colormap has been initialized
    common colormap_initialized_ 0
}




# E.S.O. - VLT project 
# "@(#) $Id$"
#
# RtdImageCtrl.tcl - Widget combining an RtdImage with a control panel
#                    zoom and panning windows.
# 
# See man page RtdImageCtrl(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
# P.Biereichel    05/08/96   Added itk_option(-with_warp)
# Peter W. Draper 30 Sep 97  Converted to derive GaiaImage.
# Peter W. Draper 09 Mar 98  Colorramp is now told name of main image
#                            "the viewmaster"
# Peter W. Draper 24 Apr 98  Removed itk_option(-with_warp), this is
#                            already defined in RtdImage.

itk::usual RtdImageCtrl {}

# RtdImageCtrl is an itcl widget combining the RtdImage itcl widget with
# a control panel, zoom and panning windows. 
# 
# RtdImageCtrl inherits all of the features described in
# RtdImage(n) and also adds the following user interface components:
# Zoom window (see RtdImageZoomView(n)), Panning window (see
# RtdImagePan(n)), Colormap display widget (see RtdImageColorRamp(n)),
# Image Control panel (see RtdImagePanel(n)).
# 
# Each of the added user interface components is defined in a separate
# [incr Tk] widget class. In addition, some methods from RtdImage are
# redefined in order to update the user interface display.

class rtd::RtdImageCtrl {
    inherit gaia::GaiaImage

    #  constructor: create a new instance of this class

    constructor {args} {
	# register with colormap handler window if it is already there (clones)
	rtd::RtdImageColors::add_image $this

	# set the colormap on this window to the one used by the rtdimage
	# and arrange for all other top level windows to use that colormap
	rtd_set_cmap $w_
	rtd_set_cmap [winfo toplevel $w_]
	TopLevelWidget::set_command "rtd_set_cmap"
	
	# see "init" method for layout
	eval itk_initialize $args
    }

    # destructor 
    
    destructor {
	rtd::RtdImageColors::remove_image $this
    }

    
    # this method is called from the base class (TopLevelWidget) after all
    # the options have been evaluated

    method init {} {
	RtdImage::init
	
	feedback "control panel..."
	make_control_panel

	if ($itk_option(-with_colorramp)) {
	    feedback "color ramp..."
	    make_colorramp
	}
	
	# the "measure band" is displayed while the right mouse button 
	# is pressed to show the distance between points
	rtd::RtdImageMBand $w_.mband -image $this

	# Add mouse bindings to select a region in the image
	$canvas_ bind $imageId_ <Control-1> \
	    "+$itk_component(draw) set_drawing_mode region"

	# set short help message to be displayed whenever
	# the mouse enters the image window (see Toplevel.tcl)
	config -shelp "image window: \
                 {bitmap b1} = select graphics,\
                 Control {bitmap dragb1} = select region,\
                 {bitmap dragb2} = drag scroll,\
                 {bitmap dragb3} = measure world coordinates"

	# set up remote control
	if {[catch {$image_ remote $itk_option(-port)} msg]} {
	    error_dialog $msg $w_
	}
	
	if {"$itk_option(-file)" == ""} {
	    after 0 [code $this clear]
	}
    }

    
    # make the control panel for operating on the image
    
    method make_control_panel {} {
	itk_component add panel {
	    set panel [frame $w_.panel]
	}
	pack $panel -side top -fill x -before $w_.imagef
	
	# zoom window
	if {$itk_option(-with_zoom_window)} {
	    feedback "zoom window..."
	    make_zoom_window $panel
	}

	# add info panel
	feedback "info panel..."
	itk_component add info {
	    rtd::RtdImagePanel $w_.info \
		-image $this \
		-state disabled \
		-min_scale $itk_option(-min_scale) \
		-max_scale $itk_option(-max_scale) \
		-shorthelpwin $itk_option(-shorthelpwin) \
		-borderwidth 3 -relief groove
	}
	pack $itk_component(info)  \
	    -side left -fill both -expand 1 -in $panel
	
	# add an item to control the grid size
	if {$itk_option(-with_grid)} {
	    make_grid_item
	}

	# panning window
	if {$itk_option(-with_pan_window)} {
	    feedback "pan window..."
	    itk_component add pan {
		rtd::RtdImagePan $w_.pan \
		    -target_image $this \
		    -width $itk_option(-pan_width) \
		    -height $itk_option(-pan_height) \
		    -usexshm $itk_option(-usexshm) \
                    -usexsync $itk_option(-usexsync) \
		    -verbose $itk_option(-verbose) \
		    -borderwidth 3 \
		    -relief groove
	    }
	    pack $itk_component(pan) -side right -fill y -in $panel
	}
    }

    
    # make the zoom window in the panel

    method make_zoom_window {panel} {
	# set on/off by default
	global ::$w_.zoom.dozoom
	set $w_.zoom.dozoom $itk_option(-dozoom)

	if {$itk_option(-use_zoom_view)} {
	    itk_component add zoom {
		rtd::RtdImageZoomView $w_.zoom \
		    -target_image $this \
		    -verbose $itk_option(-verbose) \
		    -width $itk_option(-zoom_width) \
		    -height $itk_option(-zoom_height) \
		    -factor $itk_option(-zoom_factor) \
		    -propagate $itk_option(-zoom_view_propagate) \
		    -usexshm $itk_option(-usexshm) \
                    -usexsync $itk_option(-usexsync) \
		    -borderwidth 3 \
		    -relief groove
	    }
	} else {
	    # this version is not really supported any more...
	    itk_component add zoom {
		rtd::RtdImageZoom $w_.zoom \
		    -target_image $this \
		    -width $itk_option(-zoom_width) \
		    -height $itk_option(-zoom_height) \
		    -factor $itk_option(-zoom_factor) \
		    -usexshm $itk_option(-usexshm) \
                    -usexsync $itk_option(-usexsync) \
		    -borderwidth 3 \
		    -relief groove
	    }
	}
	pack $itk_component(zoom) -side left -fill y -in $panel

	# tell the base class to use this zoom window when entered
	config -zoomwin $itk_component(zoom)
    }


    # create an item in the panel to control the ra,dec grid size
    
    method make_grid_item {} {
	pack [frame $w_.gridf] \
	    -in [$itk_component(info) component lrframe] \
	    -anchor se -padx 1m -pady 1m -fill x 

	itk_component add gridcheck {
	    checkbutton $w_.gridcheck \
		-text "Grid:" \
		-font [$w_.info cget -labelfont] \
		-variable $w_.grid -command [code $this show_grid $w_.grid]
	}
	pack $itk_component(gridcheck) -side left -in $w_.gridf

	itk_component add gridsize  {
	    LabelEntry $w_.gridsize \
		-value 60 \
		-relief sunken \
		-anchor w \
		-valuewidth [$w_.info cget -valuewidth] \
		-valuefont [$w_.info cget -valuefont] \
		-command [code $this set_grid_size] \
		-validate numeric
	}
	pack $itk_component(gridsize) -side left -in $w_.gridf

	add_short_help $itk_component(gridcheck) \
	    {Grid: Toggle the visibility of the image ra,dec grid}
	add_short_help $itk_component(gridsize) \
	    {Grid Size: set space between grid lines in arcseconds of degrees}

	$w_.gridsize configure -state disabled
    }


    # toggle the visibility of the control panel 
    # (argument is the name of the checkbutton variable to use)
    
    method hide_control_panel {variable} {
	global ::$variable ::$w_.zoom.dozoom

	if {[set $variable]} {
	    # hide the panel, turn off the zoom window, keep the canvas width
	    set zoom_state_ [set $w_.zoom.dozoom]
	    set $w_.zoom.dozoom 0
	    set w [winfo width $canvas_]
	    pack forget $itk_component(panel)
	    $canvas_ config -width $w
	} else {
	    # show the panel, restore the zoom window
	    set $w_.zoom.dozoom $zoom_state_
	    pack $itk_component(panel) -side top -fill x -before $w_.imagef
	}
    }


    # toggle the visibility of the image ra,dec grid
    # (argument is the name of the checkbutton variable to use)
    
    method show_grid {variable} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	set grid_var_ $variable
	global ::$grid_var_
 
	if {[set $grid_var_]} {
	    # show the grid
	    if {![winfo exists $w_.grid]} {
		rtd::RtdImageGrid $w_.grid -image $this
	    }
	    $w_.grid show
	    $w_.gridsize configure -state normal -value [$w_.grid size]
	} else {
	    # don't show the grid
	    if {[winfo exists $w_.grid]} {
		$w_.grid hide
	    }
	    $w_.gridsize configure -state disabled
	}
    }

    
    # set the size of the grid (space between lines) in arc seconds of dec degrees

    method set_grid_size {size} {
	if {"$size" != ""} {
	    if {[catch {set size [expr int($size)]}] || $size <= 1} {
		error_dialog "Please enter the grid size as a positive number in arcsecs of degrees"
		return
	    }
	}
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	if {[winfo exists $w_.grid]} {
	    global ::$grid_var_
	    if {[info exists $grid_var_] && [set $grid_var_]} {
		$w_.grid configure -size $size
		$w_.grid reset
	    }
	}
    }
        

    # add a generated image to display the colors in the colormap

    method make_colorramp {} {
	itk_component add colorramp {
	    rtd::RtdImageColorRamp $w_.colorramp \
		-height $itk_option(-colorramp_height) \
                -viewmaster $image_
	}
	pack $itk_component(colorramp) -side bottom -fill x
    }

    
    # This method is redefined here to update the scale display.
    # resize the image and the canvas graphics by the given integer factors
    # (1 is no scale, -2 = 50%, 2 = 200% etc...)
    
    method scale {x y} {
	RtdImage::scale $x $y
	if {[info exists itk_component(zoom)]} {
	    $itk_component(zoom) scale
	}
	$itk_component(info).trans update_trans
    }

    
    # add the given increment to the current zoom factor and re-scale
    # the image
    
    method inc_zoom {inc} {
	$itk_component(info).trans inc_zoom $inc
    }

    
    # this method is called by the image code whenever a new image is loaded.
    # (for real-time updates, see camera command)

    method new_image_cmd {} {
	RtdImage::new_image_cmd
	if {[info exists itk_component(zoom)]} {
	    $itk_component(zoom) zoom
	    $itk_component(zoom) scale
	}
	if {! [$image_ isclear]} {
	    $itk_component(info) config -state normal
	}
	$itk_component(info) updateValues
	if {[winfo exists $w_.cut]} {
	    $w_.cut update_graph
	}

	if {[winfo exists $w_.spectrum]} {
	    destroy $w_.spectrum
	}

	if {[winfo exists $w_.draw]} {
	    $w_.draw set_menu_state normal
	}

	if {[winfo exists $w_.wcs_info]} {
	    $w_.wcs_info configure -values [$image_ wcsset]
	}
	
	if {[winfo exists $w_.grid]} {
	    # update grid, if on
	    global ::$grid_var_
	    if {[info exists $grid_var_] && [set $grid_var_]} {
		# reset size, since new image may have a much different scale
		# setting size to {} means it will be chosen based on the image size...
		$w_.grid configure -size {}
		$w_.grid reset
		$w_.gridsize configure -state normal -value [$w_.grid size]
	    }
	}
    }


    # open and load a new FITS image file via file name dialog
    
    method open {{dir "."} {pattern "*.*fits"}} {
	set file [filename_dialog $dir $pattern $w_]
	if {"$file" != ""} {
	    if {[file isfile $file]} {
		config -file $file
	    } else {
		error_dialog "There is no file named '$file'" $w_
	    }
	}
    }
    
    
    # view the FITS header in a text window

    method view_fits_header {} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	set s [$image_ object]
	if {"$s" == ""} {
	    set s [$image_ cget -file]
	}
	if {"$s" == ""} {
	    set s "FITS Header"
	} else {
	    set s "FITS Header for $s"
	}
	utilReUseWidget util::TextDialog $w_.fits_header \
	    -bitmap {} \
	    -textwidth 80 \
	    -buttons "Close" \
	    -modal 0 \
	    -text "$s" \
	    -messagewidth 5i \
	    -contents [$image_ fits get]
	$w_.fits_header activate
    }

    
    # pop up a dialog to display/edit the basic world coordinate 
    # parameters
    
    method wcs_info_dialog {} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	utilReUseWidget EntryForm $w_.wcs_info \
	    -title "Basic WCS Information" \
	    -labels [list \
			 "Center right ascension (H:M:S):" \
			 "Center declination (D:M:S)" \
			 "Number of arcseconds per pixel:" \
			 "Reference pixel X coordinate:" \
			 "Reference pixel Y coordinate:" \
			 "Number of pixels along x-axis:" \
			 "Number of pixels along y-axis:"  \
			 "Rotation angle (clockwise positive) in degrees:"  \
			 "Equinox (1950 and 2000 supported):"  \
			 "Epoch (for FK4/FK5 conversion, no effect if 0):" \
			 "Projection:"
			] \
	    -values [$image_ wcsset] \
	    -command [code $this set_wcs_info]
    }

    
    # This command is called with a list of values from wcs_info_dialog above
    # to set new world coordinates information for the current image.

    method set_wcs_info {list} {
	if {[catch "$image_ wcsset $list" msg]} {
	    error_dialog $msg $w_
	}
    }


    # pop up a window to edit the image colors

    method set_colors {} {
	# note that this window must be shared by all instances
	utilReUseWidget rtd::RtdImageColors .colors \
	    -image $this \
	    -shorthelpwin $itk_option(-shorthelpwin) \
	    -cmap_dir $itk_option(-cmap_dir) \
	    -cmap_suffix $itk_option(-cmap_suffix) \
	    -itt_suffix $itk_option(-itt_suffix) \
	    -default_cmap $itk_option(-default_cmap) \
	    -default_itt $itk_option(-default_itt)
	.colors update_allocated

	wm transient .colors [winfo toplevel $w_]
    }


    # This method is called when the colormap has been changed to
    # update the display

    method update_colors {} {
	if {$itk_option(-with_colorramp)} {
	    $itk_component(colorramp) update_colors
	}
    }
    
   
    # clear the current image display and remove an windows that
    # access it (extend parent class version)

    method clear {} {
	RtdImage::clear
	$itk_component(info) config -state disabled
	if {[winfo exists $w_.cut]} {
	    destroy $w_.cut
	} 
	if {[winfo exists $w_.draw]} {
	    $w_.draw set_menu_state disabled
	    wm withdraw $w_.draw
	} 
	if {[winfo exists $w_.grid]} {
	    $w_.grid hide
	    $w_.gridsize configure -state disabled
	} 
    }

    
    # this method is called at startup to give feedback while building the interface
    
    method feedback {msg} {
	eval $itk_option(-feedback) [list $msg]
    }

    
    
    # -- options --
    
    # width of zoom window
    itk_option define -zoom_width zoom_width Zoom_width 152

    # height of zoom window
    itk_option define -zoom_height zoom_height Zoom_height 152

    # zooming factor
    itk_option define -zoom_factor zoom_factor Zoom_factor 4

    # width of panning window
    itk_option define -pan_width pan_width Pan_width 152

    # height of panning window
    itk_option define -pan_height pan_height Pan_height 152

    # height of the colorramp subwindow
    itk_option define -colorramp_height colorramp_height Colorramp_height 12

    # flag: if true (default), show the color ramp window
    itk_option define -with_colorramp with_colorramp With_colorramp 1


    # flag: if true, make zoom window a view of the main image
    # otherwise do a faster, but less accurate (by shrunken images) zoom
    # from the xImage
    itk_option define -use_zoom_view use_zoom_view Use_zoom_view 1
    
    # flag: if true, changes in main image scale will propagate to the zoom window,
    # otherwise controls are displayed so the user can manually change it (ZoomView only)
    itk_option define -zoom_view_propagate zoom_view_propagate Zoom_view_propagate 1

    # flag: if true (default) make a zoom window 
    itk_option define -with_zoom_window with_zoom_window With_zoom_window 1

    # flag: if true (default) make a panning window
    itk_option define -with_pan_window with_pan_window With_pan_window 1

    # flag: if true, turn on zoom window
    itk_option define -dozoom dozoom Dozoom 1

    # default cmap file
    itk_option define -default_cmap default_cmap Default_cmap {real}

    # default ITT file
    itk_option define -default_itt default_itt Default_itt {ramp}

    # default port for remote connections (0 means system chooses a port)
    itk_option define -port port Port 0

    # command used to display feedback during startup
    itk_option define -feedback feedback Feedback "#"

    # option to warp the mouse pointer
#    itk_option define -with_warp with_warp With_warp 0

    # option to include grid button (default to off, since it doesn't work
    # well yet on some images)
    itk_option define -with_grid with_grid With_grid 0

    # minimum allowed scale value
    itk_option define -min_scale min_scale Min_scale -5

    # maximum allowed scale value
    itk_option define -max_scale max_scale Max_scale 9

    # -- protected vars --

    # panning image
    protected variable panimage_

    # panning canvas window
    protected variable panwin_
    
    # zoom frame
    protected variable zoom_

    # saved zoom button state
    protected variable zoom_state_ 0

    # cut values frame
    protected variable cut_

    # name of trace var for grid
    protected variable grid_var_
}

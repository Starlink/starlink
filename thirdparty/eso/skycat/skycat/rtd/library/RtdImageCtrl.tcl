# E.S.O. - VLT project 
# "@(#) $Id: RtdImageCtrl.tcl,v 1.1.1.1 2006/01/12 16:38:06 abrighto Exp $"
#
# RtdImageCtrl.tcl - Widget combining an RtdImage with a control panel
#                    zoom and panning windows.
# 
# See man page RtdImageCtrl(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01/06/95   Created
# P.Biereichel    05/08/96   Added itk_option(-with_warp)
# A.Brighton      22/03/98   Added -float_panel option to allow info panel to be
#                            in a popup window (default is off, no change),
#                            added -panel_layout option to allow changing the
#                            order of the zoom and pan windows in the layout
#                            (default is the same as before, no change).
# P.W.Draper      22/01/99   Added -viewmaster option to RtdImageColorRamp
#                            instance.
# P.Biereichel    22/03/99   Added handling of bias widget
# P.Biereichel    29/06/99   Added HDU code (copied from skycat)
# pbiereic        25/05/00   Added method 'autoscale'
# pbiereic        16/08/01   Added method 'reopen' to update FITS HDU's
#                            Adapted for new widget RtdImageFitsHeader for viewing
#                            FITS HDU headers.
# P.W.Draper      05/12/06   Allow autoscale of image whose dimensions are 1.

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

itcl::class rtd::RtdImageCtrl {
    inherit rtd::RtdImage

    #  constructor: create a new instance of this class

    constructor {args} {
	# register with colormap handler window if it is already there (clones)
	rtd::RtdImageColors::add_image $this
        # register with bias image handler
        rtd::RtdImageBias::add_image $this

	# set the colormap on this window to the one used by the rtdimage
	# and arrange for all other top level windows to use that colormap
	rtd_set_cmap $w_
	rtd_set_cmap [winfo toplevel $w_]
	util::TopLevelWidget::set_command "rtd_set_cmap"
	
	# see "init" method for layout
	eval itk_initialize $args
    }

    # destructor 
    
    destructor {
	rtd::RtdImageColors::remove_image $this
        rtd::RtdImageBias::remove_image $this
    }

    
    # this method is called from the base class (TopLevelWidget) after all
    # the options have been evaluated

    protected method init {} {
	RtdImage::init
	
	feedback "control panel..."
	make_control_panel

	if ($itk_option(-with_colorramp)) {
	    feedback "color ramp..."
	    make_colorramp
	}
	
	# the "measure band" is displayed while the right mouse button 
	# is pressed to show the distance between points
	rtd::RtdImageMBand $w_.mband \
	    -image $this \
	    -defaultcursor $itk_option(-cursor)


	# Add mouse bindings to select a region in the image
	$canvas_ bind $imageId_ <Control-1> \
	    "+$itk_component(draw) set_drawing_mode region"

	# set short help message to be displayed whenever
	# the mouse enters the image window (see Toplevel.tcl)
	set msg "image: \
                 {bitmap b1} = select object,\
                 {bitmap dragb2} = scroll image,\
                 {bitmap dragb3} = measure WCS,  \
                 Control {bitmap dragb1} = select region"
	config -shelp $msg
	#set w [winfo toplevel $w_]
	#$canvas_ bind $imageId_ <Enter> "+[code $w_ short_help $msg]"
	#$canvas_ bind $imageId_ <Leave> "+[code $w_ short_help {}]"

	# set up remote control
	if {[catch {$image_ remote $itk_option(-port)} msg]} {
	    error_dialog $msg $w_
	}
	
	if {"$itk_option(-file)" == ""} {
	    after 0 [code $this clear]
	}

	if { $itk_option(-float_panel) } {
	    after 0 [code wm deiconify $itk_component(panel)]
	}
    }

    
    # make the control panel for operating on the image
    
    protected method make_control_panel {} {
	if { $itk_option(-float_panel) } {
	    # The RTD control panel, may be put in a frame or optionally 
	    # in a popup window
	    itk_component add panel {
		set panel [TopLevelWidget $w_.panel]
	    }
	    wm withdraw $panel

	    #  Stop this window from being destroyed.
	    wm protocol $panel WM_DELETE_WINDOW {}
	    wm title $panel "Control Panel ([$panel cget -number])"
	} else {
	    itk_component add panel {
		set panel [frame $w_.panel]
	    }
	    if { "$itk_option(-panel_orient)" == "vertical" } {
		pack $panel -side left -fill y -before $w_.imagef
	    } else {
		pack $panel -side top -fill x -before $w_.imagef
	    }
	}
	
	# add the subwindows
	make_panel_subwindows $panel
    }

    
    # add the panel subwindows

    protected method make_panel_subwindows {panel} {
	switch -exact -- $itk_option(-panel_layout) {
	    saoimage {
		make_panel_info $panel
		make_pan_window $panel
		make_zoom_window $panel
	    }
	    reverse {
		make_pan_window $panel
		make_panel_info $panel
		make_zoom_window $panel
	    }
	    default {
		make_zoom_window $panel
		make_panel_info $panel
		make_pan_window $panel
	    }
	}
    }

    
    # make the panel info subwindow
    
    protected method make_panel_info {panel} {
	# add info panel
	feedback "info panel..."
	
	# Info panel, RtdImagePanel(n) object used to display image controls
	itk_component add info {
	    rtd::RtdImagePanel $panel.info \
		-image $this \
		-state disabled \
		-panel_orient $itk_option(-panel_orient) \
		-min_scale $itk_option(-min_scale) \
		-max_scale $itk_option(-max_scale) \
		-shorthelpwin $itk_option(-shorthelpwin) \
		-borderwidth 3 -relief groove
	}

	if { "$itk_option(-panel_orient)" == "vertical" } {
	    pack $itk_component(info) -side top -fill y -expand 1
	} else {
	    pack $itk_component(info) -side left -fill both -expand 1
	}
	
	# add an item to control the grid size
	if {$itk_option(-with_grid)} {
	    make_grid_item
	}

	# flash background color of object label for real-time events
	config -cameraPreCmd "$itk_component(info) flash 1" \
		-cameraPostCmd "$itk_component(info) flash 0"
    }

    # update camera status display

    public method updateCameraStatus {camera} {
	if {[info exists itk_component(info)]} {
	    $itk_component(info) updateCameraStatus $camera [$image_ camera attach]
	}
    }
    
    # make the pan window
    
    protected method make_pan_window {panel} {
	# panning window
	if {$itk_option(-with_pan_window)} {
	    feedback "pan window..."
	    # pan window (RtdImagePan(n) widget).
	    itk_component add pan {
		rtd::RtdImagePan $panel.pan \
		    -target_image $this \
		    -width $itk_option(-pan_width) \
		    -height $itk_option(-pan_height) \
		    -usexshm $itk_option(-usexshm) \
                    -usexsync $itk_option(-usexsync) \
		    -verbose $itk_option(-verbose) \
		    -borderwidth 3 \
		    -relief groove
	    }
	    if { "$itk_option(-panel_orient)" == "vertical" } {
		pack $itk_component(pan) -side top
	    } else {
		pack $itk_component(pan) -side left -fill y
	    }
	}
    }

    
    # make the zoom window in the panel

    protected method make_zoom_window {panel} {
	if {! $itk_option(-with_zoom_window)} {
	    return
	}
	feedback "zoom window..."

	# set on/off by default
	global ::$panel.zoom.dozoom
	set $panel.zoom.dozoom $itk_option(-dozoom)

	if {$itk_option(-use_zoom_view)} {
	    # Zoom window (RtdImageZoomView(n) widget)
	    itk_component add zoom {
		rtd::RtdImageZoomView $panel.zoom \
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
	    # Zoom window: this version is not really supported any more...
	    itk_component add zoom {
		rtd::RtdImageZoom $panel.zoom \
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
	if { "$itk_option(-panel_orient)" == "vertical" } {
	    pack $itk_component(zoom) -side top -fill both -expand 0
	} else {
	    pack $itk_component(zoom) -side left -fill both -expand 0
	}

	# tell the base class to use this zoom window when entered
	config -zoomwin $itk_component(zoom)
    }


    # create an item in the panel to control the ra,dec grid size
    
    protected method make_grid_item {} {
	# Frame at lower right for the grid checkbutton and size entry
	itk_component add gridf {
	    set gridf [frame [$itk_component(info) component lrframe].gridf]
	}
	pack $gridf -anchor se -padx 1m -pady 1m -fill x 

	# Check button for optional WCS grid
	itk_component add gridcheck {
	    checkbutton $gridf.gridcheck \
		-text "Grid:" \
		-font [$itk_component(info) cget -labelfont] \
		-variable $w_.grid -command [code $this show_grid $w_.grid]
	}
	pack $itk_component(gridcheck) -side left

	# LabelEntry for grid size
	itk_component add gridsize  {
	    LabelEntry $gridf.gridsize \
		-value 60 \
		-relief sunken \
		-anchor w \
		-valuewidth [$itk_component(info) cget -valuewidth] \
		-valuefont [$itk_component(info) cget -valuefont] \
		-command [code $this set_grid_size] \
		-validate real
	}
	pack $itk_component(gridsize) -side left

	add_short_help $itk_component(gridcheck) \
	    {Grid: Toggle the visibility of the image ra,dec grid}
	add_short_help $itk_component(gridsize) \
	    {Grid Size: set space between grid lines in arcseconds of degrees}

	$itk_component(gridsize) configure -state disabled
    }


    # toggle the visibility of the control panel 
    # (argument is the name of the checkbutton variable to use)
    
    public method hide_control_panel {variable} {
	set panel $itk_component(panel)
	global ::$variable ::$panel.zoom.dozoom

	if {[set $variable]} {
	    # hide the panel, turn off the zoom window, keep the canvas width
	    set zoom_state_ [set $panel.zoom.dozoom]
	    set $panel.zoom.dozoom 0
	    set w [winfo width $canvas_]
	    pack forget $panel
	    $canvas_ config -width $w
	} else {
	    # show the panel, restore the zoom window
	    set $panel.zoom.dozoom $zoom_state_
	    if { "$itk_option(-panel_orient)" == "vertical" } {
		pack $panel -side left -fill y -before $w_.imagef
	    } else {
		pack $panel -side top -fill x -before $w_.imagef
	    }
	}
    }


    # toggle the visibility of the image ra,dec grid
    # (argument is the name of the checkbutton variable to use)
    
    public method show_grid {variable} {
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
	    $itk_component(gridsize) configure -state normal -value [$w_.grid size]
	} else {
	    # don't show the grid
	    if {[winfo exists $w_.grid]} {
		$w_.grid hide
	    }
	    $itk_component(gridsize) configure -state disabled
	}
    }

    
    # set the size of the grid (space between lines) in arc seconds of dec degrees

    public method set_grid_size {size} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	if {[winfo exists $w_.grid]} {
	    global ::$grid_var_
	    if {[info exists $grid_var_] && [set $grid_var_]} {
		$w_.grid configure -size $size
		$w_.grid reset
		$itk_component(gridsize) configure -state normal -value [$w_.grid size]
	    }
	}
    }
        

    # add a generated image to display the colors in the colormap

    protected method make_colorramp {} {
	# color ramp widget (RtdImageColorRamp(n))
	itk_component add colorramp {
	    rtd::RtdImageColorRamp $w_.colorramp \
		-height $itk_option(-colorramp_height) \
		-usexshm $itk_option(-usexshm) \
                -viewmaster $image_
		
	}
	pack $itk_component(colorramp) -side bottom -fill x -before $w_.imagef
    }

    
    # This method is redefined here to update the scale display.
    # resize the image and the canvas graphics by the given integer factors
    # (1 is no scale, -2 = 50%, 2 = 200% etc...)
    
    public method scale {x y} {
	RtdImage::scale $x $y
	if {[info exists itk_component(zoom)]} {
	    $itk_component(zoom) scale
	}
	$itk_component(info) component trans update_trans
    }

    
    # add the given increment to the current zoom factor and re-scale
    # the image
    
    public method inc_zoom {inc} {
	$itk_component(info) component trans inc_zoom $inc
    }

    
    # this method is called by the image code whenever a new image is loaded.
    # (for real-time updates, see camera command)

    protected method new_image_cmd {} {

	RtdImage::new_image_cmd
        
        # display HDU list, if there are multiple HDUs
        update_fits_hdus

	if {[info exists itk_component(zoom)]} {
	    $itk_component(zoom) zoom
	    $itk_component(zoom) scale
	}
	if {! [$image_ isclear]} {
	    $itk_component(info) config -state normal
	}
        autoscale
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
	
	if {[winfo exists $w_.fits_header]} {
	    view_fits_header
	}
	
	if {[winfo exists $w_.fits_hdu_header]} {
	    if { [$image_ hdu count] < 1 } {
		destroy $w_.fits_hdu_header
	    } else {
		view_fits_hdu_header
	    }
	}
	
	if {[winfo exists $w_.grid]} {
	    # update grid, if on
	    global ::$grid_var_
	    if {[info exists $grid_var_] && [set $grid_var_]} {
		# reset size, since new image may have a much different scale
		# setting size to {} means it will be chosen based on the image size...
		$w_.grid configure -size {}
		$w_.grid reset
		$itk_component(gridsize) configure -state normal -value [$w_.grid size]
	    }
	}
    }


    # open and load a new FITS image file via file name dialog
    
    public method open {{dir "."} {pattern "*.*fit*"}} {
	if { "$dir" == "." && "[cget -image_directory]" != "" } {
	    set dir [cget -image_directory]
        }
	set file [filename_dialog $dir $pattern $w_]
	if {"$file" != ""} {
	    if {[file isfile $file]} {
                detach_camera
		config -file $file
	    } else {
		error_dialog "There is no file named '$file'" $w_
	    }
	}
    }
    
    
    # view the FITS header in a text window

    public method view_fits_header {} {
	set s [$image_ object]
	if {"$s" == ""} {
	    set s [$image_ cget -file]
	}
	if {"$s" == ""} {
	    set s "FITS Header"
	} else {
	    set s "FITS Header for $s"
	}
	set existed [winfo exists $w_.fits_header]
	utilReUseWidget util::TextDialog $w_.fits_header \
	    -bitmap {} \
	    -textwidth 80 \
	    -buttons "Close" \
	    -modal 0 \
	    -text "$s" \
	    -title "$s" \
	    -messagewidth 5i \
	    -contents [$image_ fits get]
	if {! $existed} {
	    $w_.fits_header activate
	}
    }
    
    # view the FITS HDU headers

    public method view_fits_hdu_header {} {
	if { [$image_ hdu count] < 1 && "[$image_ object]" == "[cget -camera]"} { 
	    warning_dialog "No FITS header available for real-time images" $w_
	    return 
	}

        utilReUseWidget rtd::RtdImageFitsHeader $w_.fits_hdu_header \
            -image $this \
            -shorthelpwin $itk_option(-shorthelpwin)
        update idletasks
        $w_.fits_hdu_header activate
    }

    
    # pop up a dialog to display/edit the basic world coordinate 
    # parameters
    
    public method wcs_info_dialog {} {
	if {[$image_ isclear]} {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	utilReUseWidget util::EntryForm $w_.wcs_info \
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
	    -scroll 0 \
	    -command [code $this set_wcs_info]
    }

    
    # This command is called with a list of values from wcs_info_dialog above
    # to set new world coordinates information for the current image.

    public method set_wcs_info {list} {
	if {[catch "$image_ wcsset $list" msg]} {
	    error_dialog $msg $w_
	}
    }


    # pop up a window to edit the image colors

    public method set_colors {} {
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


    # Update the settings in the color popup to reflect those of the image

    public method update_color_window {} {
	if {[winfo exists .colors]} {
	    .colors update_values $image_
	}
    }
    
   
    # This method is called when the colormap has been changed to
    # update the display

    public method update_colors {} {
	if {$itk_option(-with_colorramp)} {
	    $itk_component(colorramp) update_colors
	}
    }
    

    # pop up a window to control bias subtraction

    public method set_bias {} {
        # note that this window must be shared by all instances
        utilReUseWidget rtd::RtdImageBias .bias \
            -image $this \
            -shorthelpwin $itk_option(-shorthelpwin) \
            -dsplnr [[winfo toplevel $w_] cget -number] \
            -command [code $itk_component(info) updateValues]
        wm transient .bias [winfo toplevel $w_]
    }
    
   
    # clear the current image display and remove an windows that
    # access it (extend parent class version)

    public method clear {} {
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
	    $itk_component(gridsize) configure -state disabled
	} 
    }

    
    # this method is called at startup to give feedback while building the interface
    
    public method feedback {msg} {
	eval $itk_option(-feedback) [list $msg]
    }

    # reopen file and update HDU's

    public method reopen {} {
        rtd::RtdImage::reopen
        update_fits_hdus
   }
    
    # display a popup window listing the HDUs in the current image, if any

    public method display_fits_hdus {} {
        if {[catch {set n [$image_ hdu count]}]} {
            set n 0
        }

	if {$n <= 1} {
	    warning_dialog "There are no FITS extensions" $w_
	    return
	}

        utilReUseWidget rtd::RtdImageHduChooser $w_.hdu \
	    -center 0 \
            -image $this \
            -shorthelpwin $itk_option(-shorthelpwin) \
            -usexshm $itk_option(-usexshm) \
            -usexsync $itk_option(-usexsync) \
            -verbose $itk_option(-verbose)

    }

    # Update the popup window listing the HDUs in the current image

    public method update_fits_hdus {} {
	
        if {[catch {set n [$image_ hdu count]}]} {
            set n 0
        }

        # display and hide window automatically as needed
        if {[winfo exists $w_.hdu]} {
            if {$n > 1} {
                after idle [code $this show_hdu]
            } else {
                after idle "destroy $w_.hdu"
            }
        } else {
            # if there is more than one HDU, display the HDU select window
            if {$n > 1} {
                display_fits_hdus
            }
        }
    }

    public method show_hdu {} {
        if {[winfo exists $w_.hdu]} {
	    $w_.hdu show_hdu_list
	} else {
	    display_fits_hdus
	}
    }
    
    # This method is redefined here to update the scale display when
    # 'autoscale' is set

    public method maybe_center {} {
        rtd::RtdImage::maybe_center
        maybe_autoscale
    }
    
    # This method is redefined here to update the scale display when
    # 'autoscale' is set

    public method rotate {bool} {
        rtd::RtdImage::rotate $bool
        maybe_autoscale
    }

    # auto scale image to the max. visible size

    public method autoscale { {variable ""} } {
        if {"$variable" != ""} {
            global ::$variable
            set autoscale_ [set $variable]
        }
        if {! [catch {$itk_component(info) component trans} trans]} {
            if { $autoscale_ } {
                foreach compo "larger smaller choose" {
                    catch {$trans component $compo config -state disabled}
                }
            } else {
                $trans config -state normal
            }
        }
        maybe_autoscale
    }

    # if the image does not fill the visible canvas, scale it
    #
    # XXX allan: don't use $image_ config -fillwidth ..., 
    # need to use RtdImage itcl widget so graphics are updated
    # to use the new scale.
    
    public method maybe_autoscale {} {
        if { [$image_ isclear] || ! $autoscale_} {
                $image_ configure -fillwidth 0 -fillheight 0
        } else {
            set cw [winfo width $canvas_]
            set ch [winfo height $canvas_]
	    if {[$image_ rotate]} {
		lassign "$cw $ch" ch cw
	    }
	    #$image_ configure -fillwidth $cw -fillheight $ch
	    fill_to_fit $cw $ch
	    center
        }
    }

    # Use this method instead of $image_ configure -fillwidth $cw -fillheight $ch
    # so that graphics transformations are handled correctly.
    # The arguments are the dimensions of the image canvas.

    protected method fill_to_fit {cw ch} {
	set w [$image_ width]
	set h [$image_ height]
        set factor [expr {min(150,min($cw/$w, $ch/$h))}]
	if {$factor == 0} {
	    set factor [expr {-max(($w-1)/$cw+1, ($h-1)/$ch+1)}]
	    if {$factor >= -1} {
		set factor 1
	    }
	}
	scale $factor $factor
    }


    # -- options --
    
    # Panel layout order: set to one of {saoimage reverse default}
    # to change the layout ordering of the panel windows.
    # "saoimage" puts the info first, followed by pan and zoom,
    # "reverse" reverses the default order, which is {zoom info pan}.
    itk_option define -panel_layout panel_layout Panel_layout {}

    # Panel orient: one of {horizontal vertical} (default: horizontal)
    itk_option define -panel_orient panel_orient Panel_orient {}

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
    itk_option define -with_warp with_warp With_warp 0

    # option to include grid button (default to off, since it doesn't work
    # well yet on some images)
    itk_option define -with_grid with_grid With_grid 0

    #  Floating panel option (for small displays).
    itk_option define -float_panel float_panel Float_Panel 0

    # default scaling factor
    itk_option define -xscale xscale Xscale 1
    itk_option define -yscale yscale Yscale 1

    itk_option define -camera camera Camera {}

    # Default directory for loading images
    itk_option define -image_directory image_directory Image_directory {}

    # -- protected vars --

    # saved zoom button state
    protected variable zoom_state_ 0

    # cut values frame
    protected variable cut_

    # name of trace var for grid
    protected variable grid_var_

    # auto scale image
    protected variable autoscale_ 0
}

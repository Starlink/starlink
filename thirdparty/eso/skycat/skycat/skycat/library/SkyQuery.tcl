# E.S.O. - VLT project/ESO Archive
# @(#) $Id: SkyQuery.tcl,v 1.1.1.1 2006/01/12 16:41:57 abrighto Exp $
#
# SkyQuery.tcl - Widget for searching catalogs and plotting the results in an image.
#
# See man page SkyQuery(n) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton  6 Jan 98   created


itk::usual SkyQuery {}


# A SkyQuery widget is a frame containing entries for search
# options (inherited from class AstroQuery) with added support
# for plotting objects in an image.

itcl::class skycat::SkyQuery {
    inherit cat::AstroQuery

    # constructor

    constructor {args} {
	eval itk_initialize $args
    }

    
    # add (or update) the search options panel
    # (redefined from parent class AstroCat to add buttons)

    protected method add_search_options {} {
	AstroQuery::add_search_options
	
	if {[$astrocat iswcs] || [$astrocat ispix]} {
	    frame $search_opts_.buttons
	    pack \
		[set setfromimg \
		     [button $search_opts_.buttons.setfromimg \
			  -text "Set From Image" \
			  -command [code $this set_from_image]]] \
		[set selectarea \
		     [button $search_opts_.buttons.selectarea \
			  -text "Select Area..." \
			  -command [code $this select_area]]] \
		-side right -padx 1m -pady 2m
	    
	    if {$iscat_} {
		# put it on the same row with the last entry: "Max Objects"
		incr search_opts_row_ -1
	    }
	    add_search_option {} $search_opts_.buttons

	    add_short_help $setfromimg {Set default values from the current image}
	    add_short_help $selectarea \
		{Select and drag out a region of the image with mouse button 1}
	}
    }


    # Set the default search values to the center position and radius of the image,
    # (for catalogs) of width and height of image (for image servers).

    public method set_from_image {} {
	if {$iscat_} {
	    set_pos_radius [get_image_center_radius [$astrocat iswcs]]
	} else {
	    set_pos_width_height [get_image_center_width_height [$astrocat iswcs]]
	}
    }


    # return a list of values indicating the center coordinates and radius
    # of the current image.
    # If wcs_flag is 1, the return list is {ra dec equinox radius-in-arcmin},
    # otherwise {x y radius-in-pixels}. If no image is loaded, an empty
    # string is returned.

    public method get_image_center_radius {wcs_flag} {
	if {[$image_ isclear]} {
	    return
	}
	if {$wcs_flag} {
	    # using world coords 
	    set center [$image_ wcscenter]
	    if {[llength $center] >= 2} {
		lassign $center ra dec equinox
		set radius [format "%.2f" [$image_ wcsradius]]
		if {$radius} {
		    return [list $ra $dec $equinox $radius]
		}
	    }
	} else {
	    # using image coords
	    set w [$image_ width]
	    set h [$image_ height]
	    set x [format "%.2f" [expr $w/2.]]
	    set y [format "%.2f" [expr $h/2.]]
	    set radius [format "%.2f" [expr sqrt($x*$x+$y*$y)/2.]]
	    return [list $x $y $radius]
	}
    }


    # return a list of values indicating the center coordinates and the
    # width and height of the current image.
    # If wcs_flag is 1, the return list is {ra dec equinox width height},
    # width and height in arcmin,
    # otherwise {x y width height} in pixels. If no image is loaded, an empty
    # string is returned.

    public method get_image_center_width_height {wcs_flag} {
	if {[$image_ isclear]} {
	    return
	}
	if {$wcs_flag} {
	    # using world coords 
	    set center [$image_ wcscenter]
	    if {[llength $center] >= 2} {
		lassign $center ra dec equinox
		set width [$image_ wcswidth]
		set height [$image_ wcsheight]
		return [list $ra $dec $equinox $width $height]
	    }
	} else {
	    # using image coords
	    set w [$image_ width]
	    set h [$image_ height]
	    set x [format "%.2f" [expr $w/2.]]
	    set y [format "%.2f" [expr $h/2.]]
	    set width [$image_ width]
	    set height [$image_ height]
	    return [list $x $y $width $height]
	}
    }

    # Ask the user to select an area to search interactively
    # and insert the resulting radius and center pos in the
    # catalog window.

    public method select_area {} {
	if {$iscat_} {
	    set_pos_radius [select_image_area [$astrocat iswcs]]
	} else {
	    set_pos_width_height [select_image_area [$astrocat iswcs]]
	}
    }


    # convert the given input coordinates in the given input units to the
    # given output units and return a list {x y} with the new values.
    # The units may be one of {canvas image wcs deg "wcs $equinox", "deg $equinox"}

    public method convert_coords {in_x in_y in_units out_units} {
	return [$image_ convert coords $in_x $in_y $in_units {} {} $out_units]
    }


    # Ask the user to select an area of the image by dragging out a region
    # on the image return the resulting center pos and radius as a list of
    # {x y radius-pixels}, or {ra dec equinox radius-in-arcmin} if wcs_flag 
    # is 1. If we are dealing with an image server, the radius value is replaced
    # by width and height, i.e.: {ra dec equinox width height}, where width and 
    # height are in arcmin for wcs or pixels otherwise. 
    # An empty string is returned if there is no image or the user cancels 
    # the operation.

    public method select_image_area {wcs_flag} {
	if {"$image_" == ""} {
	    return
	}
	
	if {[$image_ isclear]} {
	    error_dialog "No image is currently loaded"
	    return
	}

	# get canvas coords of selected area
	set list [$skycat select_area]
	if {[llength $list] != 4} {
	    return
	}
	lassign $list x0 y0 x1 y1
	
	# get center and radius in canvas coords
	set x [expr ($x0+$x1)/2.]
	set y [expr ($y0+$y1)/2.]

	if {$wcs_flag} {
	    # using world coords 
	    set equinox [get_catalog_equinox]
	    if {[catch {
		lassign [convert_coords $x $y canvas "wcs $equinox"] ra dec
	    } msg]} {
		error_dialog "error converting canvas ($x, $y) to world coordinates: $msg" $w_
		return
	    }
	    if {$iscat_} {
		set radius [expr [$image_ wcsdist $x0 $y0 $x $y]/60.]
		return [list $ra $dec $equinox $radius]
	    } else {
		set width [expr [$image_ wcsdist $x0 $y0 $x1 $y0]/60.]
		set height [expr [$image_ wcsdist $x0 $y0 $x0 $y1]/60.]
		return [list $ra $dec $equinox $width $height]
	    }
	} else {
	    # using image coords
	    if {[catch {
		lassign [convert_coords $x $y canvas image] xi yi
	    } msg]} {
		error_dialog "error converting canvas ($x, $y) to world coordinates: $msg" $w_
		return
	    }
	    if {[catch {
		lassign [convert_coords $x0 $y0 canvas image] xi0 yi0
	    } msg]} {
		error_dialog "error converting canvas ($x0, $y0) to world coordinates: $msg" $w_
		return
	    }
	    if {[catch {
		lassign [convert_coords $x1 $y1 canvas image] xi1 yi1
	    } msg]} {
		error_dialog "error converting canvas ($x1, $y1) to world coordinates: $msg" $w_
		return
	    }
	    set w [expr abs($xi1-$xi0)]
	    set h [expr abs($yi1-$yi0)]
	    if {$iscat_} {
		set radius [expr sqrt($w*$w+$h*$h)/2.]
		return [list $x $y $radius]
	    } else {
		return [list $x $y $w $h]
	    }
	}
    }

    
    # Set the default values for the search panel entries:
    # (redefined from parent class AstroCat to set values from the image)

    public method set_default_values {} {
	AstroQuery::set_default_values
	set_from_image
    }


    # -- public variables --

    # name of SkyCat itcl widget
    public variable skycat {} {
	if {"$skycat" != ""} {
	    set image_ [$skycat get_image]
	}
    }

    # -- protected members --

    # internal rtdimage image for main image
    protected variable image_ {}
}


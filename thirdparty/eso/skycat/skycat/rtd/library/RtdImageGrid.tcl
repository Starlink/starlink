# E.S.O. - VLT project 
# "@(#) $Id: RtdImageGrid.tcl,v 1.11 1998/10/28 17:42:28 abrighto Exp $"
#
# RtdImageGrid.tcl - itcl class to display an ra,dec grid over an image.
#
# See man page RtdImageGrid(n) for a complete description.
#
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  24 Oct 96  Created

itk::usual RtdImageGrid {}

# RtdImageGrid is an itcl class to display an ra,dec grid over
# the image.

itcl::class rtd::RtdImageGrid {
    inherit util::FrameWidget

    # create a new object (the frame only ensures that this class
    # will be deleted along with its parent)

    constructor {args} {
        set canvas_ ""
	eval itk_initialize $args

	set image_ [$itk_option(-image) get_image]
	set canvas_ [$itk_option(-image) get_canvas]
    }

    
    # return ra + inc in deg

    protected method inc_ra {ra inc} {
	set ra [expr $ra+$inc]
	if {$ra >= 360} {
	    set ra [expr $ra-360]
	} elseif {$ra < 0} {
	    set ra [expr 360+$ra]
	}
	return $ra
    }

    
    # return dec + inc in deg

    protected method inc_dec {dec inc} {
	set dec [expr $dec+$inc]
	if {$dec >= 90} {
	    set dec [expr 180-$dec]
	} elseif {$dec <= -90} {
	    set dec [expr -180-$dec]
	}
	return $dec
    }


    # draw a line with the given points
    
    protected method draw_line {points} {
	if {[llength $points] >= 4} {
	    if {[catch "$canvas_ create line $points -tags {grid objects}" msg]} {
		#puts $msg
	    }
	}
    }


    # show the grid with the current settings

    public method show {} {
	if {"[$image_ wcswidth]" == ""} {
	    warning_dialog "Can't create WCS grid, image does not support world coordinates" $w_
	    return
	}
	
	# display busy cursor over image while drawing grid
	$itk_option(-image) busy [code $this draw]
    }

    
    # draw the grid based on the current settings

    protected method draw {} {

	# get image size in arcsec
	set wcsw [expr [$image_ wcswidth]*60]
	set wcsh [expr [$image_ wcsheight]*60]
	# puts "wcswidth in arcsec: $wcsw, height: $wcsh"

	# size of grid box in arcsecs, choose default is not specified
	# or if specified value would generate too many lines (and be slow
	# to draw).
	set maxlines 20
	set minlines 10
	set size_ $itk_option(-size)
	if {"$size_" == ""} {
	    set size_ 60
	}
	
	while {$wcsw/$size_ > $maxlines} {
	    set size_ [expr $size_*2]
	}
	while {$wcsw/$size_ < $minlines} {
	    set size_ [expr $size_/2]
	    if {"$size_" <= 0.0} {
		return
	    }
	}
	
	# size in deg
	set size_deg [expr $size_/3600.]

	# get image equinox
	set equinox [$image_ wcsequinox]
	set deg_eq  "deg $equinox"

	# start at center of image (in deg)
	lassign [$image_ wcscenter -format 1] ra0 dec0

	# check if at north or south pole, since that is a special case
	if {90-abs($dec0) < $wcsh/3600} {
	    # at pole
	    set at_pole 1
	    set nx 13 
	    set ny [expr int($wcsh/$size_+1)]
	    set ra0 0
	    set ra_inc [expr 360./($nx-1)]
	    if {$dec0 < 0} {
		# puts "at south pole"
		set dec0 -90
		set dec_inc $size_deg
	    } else {
		# puts "at north pole"
		set dec0 90
		set dec_inc -$size_deg
	    }
	    # only need to inc in one direction
	    set ra_inc_list $ra_inc
	    set dec_inc_list $dec_inc
	} else {
	    # not at pole

	    # get max number of ra,dec lines
	    set nx [expr int($wcsw/$size_+1)]
	    set ny [expr int($wcsh/$size_+1)]

	    # round ra and dec to $size border
	    set ra_inc $size_deg
	    set ra_sec [expr int($ra0*3600.)]
	    set ra_sec [expr $ra_sec + ($size_ - fmod($ra_sec,$size_))]
	    set ra0 [expr $ra_sec/3600.]

	    set dec_inc $size_deg
	    set dec_sec [expr int($dec0*3600.)]
	    set dec_sec [expr $dec_sec + ($size_ - fmod($dec_sec,$size_))]
	    set dec0 [expr $dec_sec/3600.]
	
	    # make the ra increment relative to dec0 (ra changes faster as dec nears the pole)
	    set ra_inc [expr $ra_inc/cos(($dec0/180.)*$pi_)]

	    # start in the center and inc in both directions
	    set ra_inc_list "$ra_inc -$ra_inc"
	    set dec_inc_list "$dec_inc -$dec_inc"
	}

	# puts "grid size_ = $size_, nx = $nx, ny = $ny, ra_inc = $ra_inc, dec_inc = $dec_inc"

	# draw the grid

	# lines along ra (start in center and extend in both directions)
	foreach ra_inc $ra_inc_list {
	    foreach dec_inc $dec_inc_list {
		set dec $dec0
		for {set i 0} {$i < $ny} {incr i} {
		    set ra $ra0
		    set points {}
		    for {set j 0} {$j < $nx} {incr j} {
			if {[catch {$image_ convert coords $ra $dec $deg_eq x y canvas}]} {
			    set points [draw_line $points]
			} else {
			    append points " $x $y"
			}
			set ra [inc_ra $ra $ra_inc]
		    }
		    set points [draw_line $points]
		    set dec [inc_dec $dec $dec_inc]
		}
	    }
	}

	# lines along dec
	foreach ra_inc $ra_inc_list {
	    foreach dec_inc $dec_inc_list {
		set ra $ra0
		for {set i 0} {$i < $nx} {incr i} {
		    set dec $dec0
		    set points {}
		    for {set j 0} {$j < $ny} {incr j} {
			if {[catch {$image_ convert coords $ra $dec $deg_eq x y canvas}]} {
			    set points [draw_line $points]
			} else {
			    append points " $x $y"
			} 
			set dec [inc_dec $dec $dec_inc]
		    }
		    set points [draw_line $points]
		    set ra [inc_ra $ra $ra_inc]
		}
	    }
	}
	
	$canvas_ itemconfigure grid \
	    -fill $itk_option(-color)
    }


    # hide (stop showing) the grid

    public method hide {} {
	$canvas_ delete grid
    }
    

    # reset the grid (redraw it, if needed, probably for a new image)

    public method reset {} {
	hide
	show
    }

    
    # return the actual size of the grid (space between lines) in arcsecs of degrees

    public method size {} {
	return $size_
    }

        
    # -- options --

    # main RtdImage widget (set by caller)
    itk_option define -image image Image {}
    
    # grid spacing, size of a grid box in arcsecs
    # if not specified, a default is chosen based on the image
    itk_option define -size size Size {}

    # grid line color
    itk_option define -color color Color white {
	if {$canvas_ != ""} {
  	  $canvas_ itemconfigure grid -fill $itk_option(-color)
        }
    }


    # -- protected vars -- 

    # internal rtdimage widget for main image
    protected variable image_

    # canvas window for image
    protected variable canvas_

    # const PI
    protected variable pi_ 3.14159265358979323846

    # current size of grid in arc secs of deg (same as -size if specified, 
    # otherwise it is calculated based on the size of the image)
    protected variable size_ 60
}

# E.S.O. - VLT project 
# "@(#) $Id: RtdImageColorRamp.tcl,v 1.11 1998/10/28 17:42:27 abrighto Exp $"
#
# RtdImageColorRamp.tcl - itcl widget used to display contents of the 
#                         colormap for an RtdImage in a generated image
# 
# See man page RtdImageColorRamp(n) for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
# Peter W. Draper 06 Mar 98  Added viewmaster and associated changes
#                            to support non-pseudocolor visuals
#                            (changes to colorramp do not get to other
#                            images "automagically" any more. 


itk::usual RtdImageColorRamp {}

# This [incr Tk] widget class displays the colors in the colormap from
# left to right in a generated rtdimage. In addition, bindings are added
# to the colorramp to rotate, shift, stretch and squeeze the colormap by
# dragging the mouse pointer with a button pressed.

itcl::class rtd::RtdImageColorRamp {
    inherit util::FrameWidget


    # constructor: create an RtdImage widget with generated
    # data to display all the values in the colormap

    constructor {args} {
	itk_option add hull.borderwidth hull.relief
	eval itk_initialize $args

	# RtdImage item used to display colors in colormap
	itk_component add image {
	    rtd::RtdImage $w_.image \
		-displaymode 0 \
		-usexshm $itk_option(-usexshm) \
		-borderwidth 2 \
		-relief raised \
		-scrollbars 0 \
		-drag_scroll 0 \
		-graphics 0 \
		-shelp $itk_option(-shelp) \
		-cursor $itk_option(-cursor) \
		-canvasheight $itk_option(-height)
	} {
	}
	pack $itk_component(image) -fill x

	set canvas_ [$itk_component(image) get_canvas]
	set image_ [$itk_component(image) get_image]

	# catch resize event: color ramp should always fill width of the window
	bind $canvas_ <Configure> [code $this update_colors]

	# add bindings for rotating the colormap
	# (these will change later when more functions are available)
	# Note: need shift of single color ? (see saoimage)
	bind $canvas_ <1> [code $this mark %x]
	bind $canvas_ <2> [code $this mark %x]
        bind $canvas_ <3> [code $this reset_colors]
	bind $canvas_ <B1-Motion> [code $this shift_colors %x]
	bind $canvas_ <Shift-B1-Motion> [code $this rotate_colors %x]
	bind $canvas_ <B2-Motion> [code $this scale_itt %x]
	bind $canvas_ <B3-Motion> " "
	bind $canvas_ <ButtonRelease-1> [code $this save_cmap]
	bind $canvas_ <ButtonRelease-2> [code $this save_cmap]
    }

   
    # update the colorramp after the window has been resized or the number of
    # colors has changed (need to delay to always get the correct size)

    public method update_colors {} {
	after 0 [code $image_ colorramp]
    }


    # mark the given position for later reference

    protected method mark {pos} {
	set mark_ $pos
    }

    
    # rotate the colormap by the difference between the given
    # position and the position set with mark.
    
    protected method rotate_colors {pos} {
        set val [expr $pos-$mark_]
	$image_ cmap rotate $val
        if { $itk_option(-viewmaster) != {} } {
           $itk_option(-viewmaster) cmap rotate $val
        }
	mark $pos
    }

    # shift the colormap by the difference between the given
    # position and the position set with mark.
    
    protected method shift_colors {pos} {
        set val [expr $pos-$mark_]
	$image_ cmap shift $val
        if { $itk_option(-viewmaster) != {} } {
           $itk_option(-viewmaster) cmap shift $val
        }
    }

    # scale the current ITT based on the difference between the given
    # position and the position set with mark.
    
    protected method scale_itt {pos} {
        set val [expr $pos-$mark_]
        $image_ itt scale $val
        if { $itk_option(-viewmaster) != {} } {
           $itk_option(-viewmaster) itt scale $val
        }
    }

    # reset the colormap
    method reset_colors {} {
       $image_ cmap reset
        if { $itk_option(-viewmaster) != {} } {
           $itk_option(-viewmaster) cmap reset
        }
    }
    
    # Called after a shift or scale operation is done (button up)
    # to save the colormap state. We just do a null rotate here,
    # since it does what we want. This prevents the colormap from
    # reverting to the original state before each shift or scale
    # operation. The reason it would revert is that otherwise colors
    # shifted off to the left or right, for example, would be lost.

    protected method save_cmap {} {
	$image_ cmap rotate 0
    }

    
    # -- public vars --
    
    # height of colorramp (width is same as window)
    itk_option define -height height Height 12

    # help text displayed when mouse enters widget
    itk_option define -shelp shelp Shelp "Colormap display: \
               {bitmap dragb1} = shift colormap, \
               {bitmap shiftdragb1} = rotate, \
               {bitmap dragb2} = stretch, \
               {bitmap b3} = reset"
    
    # cursor for window
    itk_option define -cursor cursor Cursor {exchange}

    # X shared memory option
    itk_option define -usexshm useXshm UseXshm 1

    # "viewmaster" image. This is also updated when colorramp
    # changes. This allows changes to be propagated, even if using a
    # read-only visual. 
    itk_option define -viewmaster viewmaster ViewMaster {}

    # -- protected vars --

    # canvas window containing ramp image
    protected variable canvas_

    # internal rtdimage widget for colorramp
    protected variable image_

    # used to save a position for rotating the colormap
    protected variable mark_ 0

}

#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImageTrans.tcl,v 1.1.1.1 2006/01/12 16:38:24 abrighto Exp $"
#
# RtdImageTrans.tcl - itcl widget for scaling, rotating and flipping an RtdImage widget
# 
# See man page RtdImageTrans(n) for a complete description.
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created

itk::usual RtdImageTrans {}

# RtdImageTrans is an [incr Tk] widget class for setting and displaying
# image transformation states, such as rotation, flipX, flipY and scale
# (magnification).  The widget displays a menubutton with a selection of
# image scale factors (from 1/5x to 9x magnification), 2 optional
# buttons for incrementing and decrementing the scale factor, and buttons
# for setting rotation, flipX and flipY.

itcl::class rtd::RtdImageTrans {
    inherit util::FrameWidget


    #  constructor: create a new instance of this class

    constructor {args} {
	itk_option add hull.borderwidth hull.relief

	# do this first so we can use the option values
	eval itk_initialize $args

	set image_ [$itk_option(-image) get_image]

	# LabelMenu(n) widget to choose scale factor
	itk_component add choose {
	    set m [util::LabelMenu $w_.choose \
		       -text "Scale:" \
		       -relief groove \
		       -labelfont $itk_option(-labelfont) \
		       -valuefont $itk_option(-valuefont) \
		       -labelwidth $itk_option(-labelwidth) \
		       -valuewidth 5 \
		       -anchor e]
	} {
	    keep -state -background -foreground
	}
	if { "$itk_option(-panel_orient)" == "vertical" } {
	    pack $itk_component(choose) -side top -fill x -ipadx 0.5m -ipady 0.5m 
	} else {
	    pack $itk_component(choose) -side left -fill x -ipadx 0.5m -ipady 0.5m 
	}
	   
	# help text displayed when mouse enters widget
	add_short_help $w_ {Image Scale: {bitmap b1} = select the magnification of the image}

	frame $w_.trans_frame
	if {$itk_option(-show_Zz_buttons)} {

	    # Tk button to zoom in
	    itk_component add larger {
		button $w_.larger \
		    -bitmap magnify \
		    -command [code $this inc_zoom 1]
	    } {
		keep -state -background -foreground
	    }
	    # Tk button to zoom out
	    itk_component add smaller {
		button $w_.smaller \
		    -bitmap shrink \
		    -command [code $this inc_zoom -1]
	    } {
		keep -state -background -foreground
	    }

	    pack $itk_component(larger) $itk_component(smaller) \
		 -side left -fill x -padx 0.5m -ipadx 0.5m -ipady 0.5m -in $w_.trans_frame

	    add_short_help $itk_component(larger)  {Zoom larger: {bitmap b1} = zoom in on the image}
	    add_short_help $itk_component(smaller) {Zoom smaller: {bitmap b1} = zoom out on the image}
	}

	if {$itk_option(-show_trans)} {

	    # Tk checkbutton to rotate (swap X/Y axis)
	    itk_component add rotate {
		checkbutton $w_.rotate \
		     -bitmap rotate \
		     -selectcolor {} \
		     -indicatoron 0 \
		     -variable $w_.var(rotate) \
		     -command [code $this rotate]
	    } {
		keep -state -background -foreground
	    }
	    # Tk checkbutton to flip the X axis
	    itk_component add flipx {
		checkbutton $w_.flipx \
		     -bitmap flipx \
		     -selectcolor {} \
		     -indicatoron 0 \
		     -variable $w_.var(flipx) \
		     -command [code $this flip x]
	    } {
		keep -state -background -foreground
	    }
	    # Tk checkbutton to flip the Y axis
	    itk_component add flipy {
		checkbutton $w_.flipy \
		     -bitmap flipy \
		     -selectcolor {} \
		     -indicatoron 0 \
		     -variable $w_.var(flipy) \
		     -command [code $this flip y]		
	    } {
		keep -state -background -foreground
	    }
	    pack $itk_component(rotate) \
		-side left -fill x -padx 1m -ipadx 1.5m -ipady 0.5m -in $w_.trans_frame

	    pack $itk_component(flipx) $itk_component(flipy) \
		-side left -fill x -padx 0.5m -ipadx 0.5m -ipady 0.5m -in $w_.trans_frame

	    add_short_help $itk_component(rotate) \
		{Exchange: {bitmap b1} = swap the image X and Y axes}
	    add_short_help $itk_component(flipx) \
		{Flip X: {bitmap b1} = flip the image about the X axis}
	    add_short_help $itk_component(flipy) \
		{Flip Y: {bitmap b1} = flip the image about the Y axis}
	}
	
	if { "$itk_option(-panel_orient)" == "vertical" } {
	    pack $w_.trans_frame -side top -fill x -pady 0.5m
	} else {
	    pack $w_.trans_frame -side left -fill x
	}

	for {set i $itk_option(-min_scale)} {$i<=-2} {incr i 1} {
	    $m add -label "1/[expr {-$i}]x" -command "$itk_option(-image) scale $i $i"
	}
	for {set i 1} {$i<=$itk_option(-max_scale)} {incr i} {
	    $m add -label "  ${i}x" -command "$itk_option(-image) scale $i $i"
	}
	$m config -value  {  1x}
    }


    # add the given increment to the current zoom factor and re-scale
    # the target image
    
    public method inc_zoom {inc} {
	lassign [$image_ scale] xs ys
	if {"$xs" == ""} {
	    return
	}
	incr xs $inc
	incr ys $inc
	if {$xs == 0 || $xs == -1} {
	    if {$inc == -1} {
		set xs -2
		set ys -2
	    } else {
		set xs 1
		set ys 1
	    }
	} elseif {$xs < $itk_option(-min_scale)} {
	    set xs [set ys $itk_option(-min_scale)]
	} elseif {$xs > $itk_option(-max_scale)} {
	    set xs [set ys $itk_option(-max_scale)]
	}
	
	$itk_option(-image) scale $xs $ys
	update_trans
    }

    
    # fill the given menu with radiobuttons for changing the magnification
    # of the image and keep them updated with the other controls

    public method fill_mag_menu {m} {
	global ::$w_.mag
	for {set i $itk_option(-min_scale)} {$i<=-2} {incr i 1} {
	    $m add radiobutton \
		-label "1/[expr {-$i}]x" \
		-command "$itk_option(-image) scale $i $i" \
		-variable $w_.mag
	}
	for {set i 1} {$i<=$itk_option(-max_scale)} {incr i} {
	    $m add radiobutton \
		-label "  ${i}x" \
		-command "$itk_option(-image) scale $i $i" \
		-variable $w_.mag
	}
	set $w_.mag {  1x}
    }


    # update the display based on the image scale factors
    # (note that the menu values are referenced to here by their labels)
    
    public method update_trans {} {
	global ::$w_.var ::$w_.mag

	if {$itk_option(-show_trans)} {
	    set $w_.var(rotate) [$image_ rotate]
	    set $w_.var(flipx) [$image_ flip x]
	    set $w_.var(flipy) [$image_ flip y]
	}

	lassign [$image_ scale] xs ys
	if {"$ys" == ""} {
	    return
	}
	if {$xs >= 0} {
	    $itk_component(choose) config -value [set $w_.mag "  ${xs}x"]
	} else {
	    $itk_component(choose) config -value [set $w_.mag "1/[expr {-$xs}]x"]
	}	
    }


    # toggle rotation of the image

    public method rotate {} {
	global ::$w_.var
	$itk_option(-image) rotate [set $w_.var(rotate)]
    }


    # flip or unflip the image about the x or y axis, as given by $xy

    public method flip {xy} {
	global ::$w_.var
	$itk_option(-image) flip $xy [set $w_.var(flip$xy)]
    }

        
    # -- public vars --
    
    # target RtdImage (itcl widget)
    itk_option define -image image Image {}

    # font for label and value
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*
    itk_option define -valuefont valueFont ValueFont -Adobe-helvetica-medium-r-normal-*-12*

    # set the width for  displaying the label
    itk_option define -labelwidth labelWidth LabelWidth 5
    # set the width for displaying the value
    itk_option define -valuewidth valueWidth ValueWidth 12

    # flag: if true, display buttons for zooming the image in and out
    itk_option define -show_Zz_buttons show_Zz_buttons Show_Zz_buttons 1

    # flag: if true, display the rotate, flipxy items
    itk_option define -show_trans show_trans Show_trans 1
    
    # minimum allowed scale value
    itk_option define -min_scale min_scale Min_scale -10

    # maximum allowed scale value
    itk_option define -max_scale max_scale Max_scale 20

    # set the state to normal/disabled to enable/disable editing
    itk_option define -state state State {normal}

    # Panel orient: one of {horizontal vertical} (default: horizontal)
    itk_option define -panel_orient panel_orient Panel_orient {}


    # -- protected vars --
     
    # internal rtdimage
    protected variable image_

}

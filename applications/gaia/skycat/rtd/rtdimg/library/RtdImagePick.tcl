# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: RtdImagePick.tcl,v 1.23 1998/10/28 17:42:29 abrighto Exp $"
#
# RtdImagePick.tcl - widget to select an object in an image using a centroid alg.
#
# See man page RtdImagePick(n) for a complete description.
#
# who              when      what
# --------         --------  ----------------------------------------------
# Allan Brighton   25 Jun 96  Created
# Peter Biereichel 01 Jul 97  modified for SOFI/ISAAC
# Allan Brighton   23 Apr 98  Merged in Peter's changes, which include using 
#                             RtdImageZoomView for the image.
#                             Fixed bug in "scale_changed" method (need to handle
#                             width and height, not just width of image).
#                             Added -orient option to change layout and changed
#                             default to vertical.
#                             Use init {} method, added "initialized_" flag.
#                             Fixed comments for itcldoc and added public and 
#                             protected keywords to methods (for doc).


itk::usual RtdImagePick {}


# RtdImagePick is an itcl widget to select an object (a star, for example) in an 
# image and get statistics for it. It is based on the rtdimage(3) "statistics" 
# subcommand, which uses a centroid algorithm to locate the center of the object.

itcl::class rtd::RtdImagePick {
    inherit util::TopLevelWidget


    #  constructor: create a new RtdImagePick widget

    constructor {args} {
        eval itk_initialize $args
    }


    # destructor

    destructor {
        catch {cancel}
        catch {$target_image_ view remove $image_}
    }
    

    # This method is called after the options have been evaluated.

    protected method init {} {
 	wm title $w_ "Pick Object ($itk_option(-number))"
 	wm iconname $w_ "Pick Object ($itk_option(-number))"
	make_layout
	update_scale
	incr initialized_
    }

    
    # This method is responsible for the window layout

    protected method make_layout {} {
        # main controls frame
        pack [set mainf [frame $w_.mainf]] \
            -side top -fill both -expand 1 -padx 1m -pady 1m -ipadx 1m -ipady 1m

        pack [set topf [frame $mainf.topf]] \
            -side top -fill both -expand 1

	if {"$itk_option(-orient)" == "horizontal"} {
	    make_labels $topf left
	    make_rect $topf left
	} else {
	    make_rect $topf top
	    make_labels $topf top
	}
        add_buttons
    }


    # Create the window for displaying the statistics in the given frame.
    # The optional "side" arg is used for packing.

    protected method make_labels {w {side left}} {
        # frame for labels
        pack [set labelf [frame $w.labelf -bd 2 -relief groove]] \
            -side $side -fill both -expand 0

        # title for label area
        pack [label $labelf.title -text "Image Statistics:"] \
            -side top -padx 1m -pady 1m
        
	# LabelValue(n) widget: "Image X" 
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
	# LabelValue(n) widget: "Image Y"
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
	# LabelValue(n) widget for RA (alpha)
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
	# LabelValue(n) widget for DEC (delta)
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
 	# LabelValue(n) widget for equinox
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
	# LabelValue(n) widget for "Peak object level above bg"
        itk_component add object {
            util::LabelValue $labelf.object \
                -text "Peak value above bg:" \
                -labelfont $itk_option(-labelfont) \
                -valuefont $itk_option(-valuefont) \
                -valuewidth $itk_option(-valuewidth) \
                -labelwidth $itk_option(-labelwidth) \
                -relief groove \
                -anchor e
        } 
 	# LabelValue(n) widget for "Background level"
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
	# LabelValue(n) for "FWHM X:Y"
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
 	# LabelValue(n) widget for "Angle of X axis"
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
 	# LabelValue(n) widget for number of "Pixels in x,y"
        itk_component add nsize {
            util::LabelValue $labelf.nsize \
                -text "Pixels in x,y:" \
                -labelfont $itk_option(-labelfont) \
                -valuefont $itk_option(-valuefont) \
                -valuewidth $itk_option(-valuewidth) \
                -labelwidth $itk_option(-labelwidth) \
                -relief groove \
                -anchor e
        } 

        foreach el {x y ra dec equinox object background fwhm angle nsize} {
            [$itk_component($el) component entry] config -justify right
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
            $itk_component(nsize) \
            -side top -padx 0.5m -pady 0.0m

        add_short_help $itk_component(x) \
	    {X image pixel coordinate (or X detector chip coord if known)}
        add_short_help $itk_component(y) \
	    {Y Image pixel coordinate (or Y detector chip coord if known)}
        add_short_help $itk_component(ra) \
	    {World Coordinates RA value}
        add_short_help $itk_component(dec) \
	    {World Coordinates DEC value}
        add_short_help $itk_component(equinox) \
	    {World Coordinates equinox (default: J2000)}
        add_short_help $itk_component(object) \
	    {Object: peak value of object above background}
        add_short_help $itk_component(background) \
	    {Background: mean background level}
        add_short_help $itk_component(fwhm) \
	    {FWHM: full width half maximum in X and Y}
        add_short_help $itk_component(angle) \
	    {Angle: angle of major axis, degrees, along X = 0}
        add_short_help $itk_component(nsize) \
	    {Number of pixels: along x and y-axis }
    }


    # Create the window used to display the part of the image being examined.
    # $w is the parent frame.
    # The optional "side" arg is used for packing.

    protected method make_rect {w {side left}} {
        # frame for rect
        pack [set rectf [frame $w.rectf -bd 2 -relief groove]] \
            -side $side -fill both -expand 0

        # add a frame to show the size of the square selected by the scale
        pack [label $rectf.label -text "Area of image to be examined:"] \
            -side top -padx 1m -pady 1m

        pack [set rf [frame $rectf.rf \
                          -bd 0 -relief flat \
                          -width $itk_option(-maxsize) \
                          -height $itk_option(-maxsize)]] \
            -fill none -expand 1 -anchor c

        # This component displays the section of the image that will be used for
	# the centroid algorithm and statistics.
        itk_component add zoomView {
            RtdImageZoomView $rf.zoomView \
		-target_image $itk_option(-target_image) \
		-verbose $itk_option(-verbose) \
		-width $itk_option(-maxsize) \
		-height $itk_option(-maxsize) \
		-factor $itk_option(-factor) \
		-propagate 0 \
		-usexshm 1 \
		-usexsync 1 \
		-borderwidth 2 \
		-relief groove \
		-shelp {PickImage: displays section of image being examined}
        }
        set zoomImage [$itk_component(zoomView) component image]
        set image_ [$zoomImage get_image]
        set canvas_ [$zoomImage get_canvas]

        catch {destroy [$itk_component(zoomView) component check]}
        global ::$itk_component(zoomView).dozoom
        set $itk_component(zoomView).dozoom 1
        $itk_component(zoomView) inc_zoom 0

        pack [$itk_component(zoomView) component larger] -expand 1
        pack [$itk_component(zoomView) component smaller] -expand 1
        pack $itk_component(zoomView) -anchor c -fill none -expand 0
        $itk_component(zoomView) inc_zoom 0
    }


    # add a row of dialog buttons at the bottom of the window

    protected method add_buttons {} {

	# dialog buttons frame
        itk_component add buttons {
            frame $w_.buttons -borderwidth 2 -relief groove
        }
        pack $itk_component(buttons) \
            -side top -fill x -padx 1m -pady 1m
        
	# "Pick Object" button
        itk_component add pick {
            button $w_.pick \
		-text "Pick Object" \
		-command [code $this pick_object]
        }
	# Cancel button
        itk_component add cancel {
            button $w_.cancel \
		-text "Cancel" \
		-command [code $this cancel]
        }
	# Close button
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
            {Pick Object: {bitmap b1} = select object in image and display center and other statistics}
        add_short_help $itk_component(cancel) {Cancel: cancel the current pick operation}
        add_short_help $itk_component(close) {Close: close the pick window}
    }

    
    # Let the user select a point in the image and get the statistics on
    # the area. The optional argument "parms" may be set to {x0, y0}, the
    # size of the image box. If given, pick_object returns the result without 
    # user interaction. 

    public method pick_object {{parms ""} {wait 0}} {
	if {! $initialized_} {
	    after 1000 [code $this pick_object]
	    return
	}
        cancel
        set waiting_ $wait
        $itk_component(zoomView) config -command {}
        $itk_component(zoomView) zoom
        $itk_component(zoomView) config -command [code $this scale_changed]
        $w_ configure -cursor cross
        $itk_component(pick) config -state disabled
        set_values {}

        if {"$parms" == ""} {
            # wait for user to click in image
            set list_ [pick_object_in_image]
        } else {
            lassign $parms imageX_ imageY_ n
            set scale [expr double($itk_option(-maxsize)) / $n]
            set scale [round $scale]
            lassign [$image_ scale] cscale
            $itk_component(zoomView) inc_zoom [expr $scale - $cscale]
            update
        }
        $itk_component(pick) config -state normal
        $w_ configure -cursor {}
        $target_image_ zoomview stop

        utilRaiseWindow $w_
        if { $waiting_ } { return }

        if {[llength $list_] == 10} {
            set_values $list_
        }
    }


    # This method is called to allow the user to pick an object in the main image.
    # The return value is a list of:
    # "ra dec equinox fwhmX fwhmY angle objectPeak meanBackground" 
    # as returned from the rtdimage "statistics" subcommand,
    # or an error.
    
    protected  method pick_object_in_image {} {
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

    public method picked_object {} {
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


    # This method can be called when the user has selected an object 
    # or star for the "pick_object" method. 
    # The argument should be the value returned from the rtdimage
    # "statistics" subcommand

    public method picked_wcs_object {retval} {
        global ::$w_.picked
        set $w_.picked $retval
    }


    # callback from isrtdZoomView when the scaling was changed

    protected method scale_changed {} {
        set width [set height $itk_option(-maxsize)]
        $image_ convert dist $width $height canvas nxsize nysize image
        $itk_component(nsize) config -value [format_val $nxsize]
        set_values {}
        cancel 0

        # update the view if fwhm has been calculated
        if {$imageX_ < 1 || $imageY_ < 1} { 
	    return
	}
        $image_ convert dist $width $height canvas nxsize nysize image
        set xc [expr $imageX_-($nxsize/2.0)]
        set yc [expr [$image_ height]-$imageY_-($nysize/2.0)]
        $target_image_ view update $image_ $xc $yc $width $height 0 0 0 0 image
        set list_ {}
        if { $waiting_ } {
	    return
	}
        $itk_option(-target_image) busy {
            busy {
                if {! [catch {set list_ [$image_ statistics]} msg]} {
                    if {[llength $list_] == 10} {
                        set_values $list_ 0
                    }
                }
            }
        }
    }
    
    
    # format a floating point value (which may also be empty)

    protected method format_val {val} {
        if {"$val" == ""} {
            return
        }
        return [format {%.1f} $val]
    }
    

    
    # set the values of the labels from the list (results of "pick_object" call).
    # If list is empty the labels are cleared.
    # If the list is not empty, mark the ra,dec spot in the image.

    protected method set_values {list {with_rmt 1}} {
        lassign $list x y ra dec equinox fwhmX fwhmY angle object background

	# display x,y in chip coords
	if {"$x" != "" && "$y" != ""} {
	    $target_image_ convert coords $x $y image xc yc chip
	} else {
	    set xc $x
	    set yc $y
	}

	$itk_component(x)       config -value [format_val $xc]
        $itk_component(y)       config -value [format_val $yc]
        $itk_component(ra)      config -value $ra
        $itk_component(dec)     config -value $dec
        $itk_component(equinox) config -value $equinox

        $image_ convert dist 0 $itk_option(-maxsize) canvas 0 nsize image
        $itk_component(nsize)      config -value [format_val $nsize]
        $itk_component(object)     config -value [format_val $object]
        $itk_component(background) config -value [format_val $background]

        if {"$fwhmX" == "" || $fwhmX > 0 && $fwhmY > 0} {
            $itk_component(fwhm)   config -value "[format_val $fwhmX] : [format_val $fwhmY]"
            [$itk_component(fwhm)  component entry] config -foreground black
            $itk_component(angle)  config -value [format_val $angle]
        } else {
            $itk_component(fwhm)   config -value "Can't do"
            [$itk_component(fwhm)  component entry] config -foreground red
            $itk_component(angle)  config -value "___oOo___"
        }
        if {"$x" != "" && "$y"} {
            set imageX_ $x
            set imageY_ $y
        }
        if {"$x" != "" && "$y" != "" && $fwhmX < 50 && $fwhmY < 50} {
            mark_spot $x $y $image_ $canvas_ $angle $fwhmX $fwhmY
            mark_spot $x $y $target_image_ $target_canvas_ $angle $fwhmX $fwhmY 1
            if {$set_result_ && $with_rmt} {
                if {$x < 0 || $y < 0 || $x > [$image_ width] || $y > [$image_ height]} {
                    set_result -1
                } else {
                    set_result "$x $y $fwhmX $fwhmY"
                }
                set set_result_ 0
            }
        }
    }


    # set the set_result_ variable to 1

    protected method set_result_value {} {
        set set_result_ 1
    }


    # mark the given x,y image coordinate point in the given image/canvas with
    # a cross with the given width, height (image pixels) and angle (deg).

    protected method mark_spot {imagex imagey image canvas angle width height {blink 0}} {
        # get canvas coords from image coords
        $image convert coords $imagex $imagey image xc yc canvas
        $image convert dist $width $height image w h canvas

        # get bounding box
        set dx [expr $w/2]
        set dy [expr $h/2]

        set x1 [expr $xc+$dx]
        set y1 $yc

        set x2 $xc
        set y2 [expr $yc-$dy]

        set x3 [expr $xc-$dx]
        set y3 $yc

        set x4 $xc
        set y4 [expr $yc+$dy]

        
        # rotate by angle
        # convert to radian
        set rad [expr $angle/57.2958]
        set cosa [expr cos($rad)]
        set sina [expr sin($rad)]

        set rx1 [expr $xc+$cosa*$dx]
        set ry1 [expr $y1-$sina*$dx]

        set rx3 [expr $xc-$cosa*$dx]
        set ry3 [expr $y1+$sina*$dx]

        set rx2 [expr $x2-$sina*$dy]
        set ry2 [expr $yc-$cosa*$dy]

        set rx4 [expr $x2+$sina*$dy]
        set ry4 [expr $yc+$cosa*$dy]

        set bg black
        set fg white
        set tags "mark objects"
        $canvas delete $tags
        
        $canvas create line $rx3 $ry3 $rx1 $ry1 \
            -fill $bg \
            -width 5 \
            -tags "$tags $bg"

        $canvas create line $rx2 $ry2 $rx4 $ry4 \
            -fill $bg \
            -width 5 \
            -tags "$tags $bg"

        $canvas create line $rx3 $ry3 $rx1 $ry1 \
            -fill $fg \
            -width 2 \
            -tags "$tags $fg"

        $canvas create line $rx2 $ry2 $rx4 $ry4 \
            -fill $fg \
            -width 2 \
            -tags "$tags $fg"

        if {$blink} {
            blink_mark $canvas $tags
        }
    }

    
    # This method is used to make the marker given by "tags" blink on
    # and off.
    
    protected method blink_mark {canvas tags {color 0}} {
	catch {after cancel $afterId_}
	set tag [lindex $tags 0]
	if {"[$canvas gettags $tag]" == ""} { return }
	set idx1 $color
	set cols "black white"
	set col1 [lindex $cols $idx1]
	set idx2 [expr ! $color]
	set col2 [lindex $cols $idx2]
	$canvas itemconfigure white -fill $col2
	$canvas itemconfigure black -fill $col1
	set afterId_ [after 500 [code $this blink_mark $canvas $tag $idx2]]
	return
    }

    
    # cancel the current pick operation

    public method cancel {{with_pick 1}} {
	$canvas_ delete mark
	$target_canvas_ delete mark
	if {$with_pick} {
	    cancel_pick
	    set imageX_ -1
	    set imageY_ -1
	    set waiting_ 0
	}
    }


    # close this window
    
    public method close {} {
        cancel
        if {$set_result_} {
            set set_result_ 0
            set_result -1
        }
        after idle "wm withdraw $w_"
    }

    # this method is called when the user clicks in the image to select an object
    # or star for the "pick_object" method. In this case, the x,y and ra,dec position 
    # are fixed and only the other info should be calculated (used).

    public method picked_special_object {x y ra dec equinox} {
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
    

    # cancel the wait for the pick_object method and reset the cursor

    public method cancel_pick {} {
        global ::$w_.picked
        set $w_.picked ""
    }


    # dummy (called by RtdImage.tcl which assumes that the scaling is propagated)

    public method update_scale {{fx 1} {fy 1}} {
    }


    # returns statistics after image event

    protected method update_now {} {
        global ::$w_.picked
        if [$image_ isclear] { return }
        if {! $waiting_ } {
            if {"[set $w_.picked]" == ""} { return }
        }
        set waiting_ 0
        if {[catch {set list [$image_ statistics]} msg]} {
            return
        }
        $canvas_ delete mark
        $target_canvas_ delete mark
        set_values $list
    }


    # -- options --

    # target (main) RtdImage itcl widget
    itk_option define -target_image target_image Target_image {} {
        set img [cget -target_image]
        set target_image_ [$img get_image]
        set target_canvas_ [$img get_canvas]
    }

    # X shared memory option
    itk_option define -usexshm useXshm UseXshm 1

    # X synchronisation option
    itk_option define -usexsync useXsync UseXsync 1

    # flag: if true, print diagnostic messages
    itk_option define -verbose verbose Verbose {0}

    # font to use for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal--12*

    # font to use for values
    itk_option define -valuefont valueFont ValueFont -Adobe-helvetica-medium-r-normal--12*

    # font to use for ra,dec labels (alpha, delta)
    itk_option define -wcsfont wcsFont WcsFont -*-symbol-*-*-*-*-14-*-*-*-*-*-*-*

    # set the width for  displaying labels and values
    itk_option define -labelwidth labelWidth LabelWidth 18

    # set the width for  displaying labels and values
    itk_option define -valuewidth valueWidth ValueWidth 12

    # set the max size of the image sample area (screen dist)
    itk_option define -maxsize maxSize MaxSize 200

    # command to evaluate when a selection is made or canceled
    itk_option define -command command Command {}

    # debugging flag
    itk_option define -debug debug Debug 0
    
    # default zoom magnification factor
    itk_option define -factor factor Factor {10} {
        set factor_ [cget -factor]
    }

    # Specify the orientation of image and panel, one of {vertical horizontal}
    itk_option define -orient orient Orient "vertical"


    # -- protected vars -- 
    
    # internal target image
    protected variable target_image_

    # internal target canvas
    protected variable target_canvas_
    
    # internal canvas widget
    protected variable canvas_

    # internal zoomView rtd image
    protected variable image_
    
    protected variable set_result_ 0

    # X coord of last picked object
    protected variable imageX_ -1

    # Y coord of last picked object
    protected variable imageY_ -1

    # output of last statistics command after scaling and pick object command
    protected variable list_

    # id for blink after job
    protected variable afterId_

    # waiting for image event before returning result
    protected variable waiting_ 0

    # set to 1 after init {} was called
    protected variable initialized_ 0
}


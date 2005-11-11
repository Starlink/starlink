#*************************************************************************
# E.S.O. - VLT project/ ESO Archive
#
# "@(#) $Id: RtdImagePick2.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# RtdImagePick2.tcl - Pick object with a given sample size
#
# See the man page for a complete description.
#
# who       when      what
# --------  --------  ----------------------------------------------
# pbiereic  01/07/01  created

# RtdImagePick2 is used to pick an object with a given sample size which 
# can be changed interactively with a slider.
# Two pick modes are implemented: pick object and pick cursor (that's why
# it is called RtdImagePick2).
# Pick object uses a centroiding algorithm for finding the object center
# and displays the statistics likewise RtdImagePick does.
# Pick cursor simply displays the picked x, y coordinates.
#
# The itk option -pickedCmd is evaluated whenever an object was picked.
# It returns a tcl list with the following values:
#
#   { x y ra dec equinox fwhmX fwhmY angle peak background } { nsamples }
#
# The command option -newSizeCmd is evaluated whenever the sample
# size is changed. It returns the number of samples.
#
# The command option -newImageCmd is evaluated whenever a new image (i.e. an image with
# a different type or size) is loaded and also when it is cleared.
#
# RtdImagePick2 uses the second zoom provided by RTD via class RtdImagePickView.
#
# It can co-exist with RtdImagePick, i.e. it checks the X cursor font to
# determine if the click in the canvas is assigned to this widget.
#
# RtdImagePick2 listens to image events which are either a new image or
# real-time image events. When the option -postImageEvent is true then
# the statistics is updated with every new image event (which may slow down
# the display rate).
#
# Applications (like tcscam) can inherit this class and add their own widgets.
# With method 'add_my_button' one can add a control button.
# The main frame components are public (i.e. the component name will not
# change). They are:
#
#  $itk_component(infof)    - info frame
#  $itk_component(zoomf)    - zoom frame
#  $itk_component(sliderf)  - slider frame
#  $itk_component(choicef)  - choice frame
#  $itk_component(buttonf)  - button frame
#
# The frames (and all other widgets) are packed with the blt::table geometry
# manager. The 'pack' geometry manager should not be used (see LIMITATIONS
# on man page of table(BLT 2.4).
#
# There are options like -with_info, -with_choice which are set to 1 by default
# to show all frames (see options). They can be set to 0 and the application
# can re-arrange the frames (via blt::table) as required.
#
# NOTES
# - In contrast to RtdImagePick, RtdImagePick2 does not use tkwait which may
#   cause problem for an inheriting class due to nested calls to tkwait.

itk::usual RtdImagePick2 { }

itcl::class rtd::RtdImagePick2 {
    inherit util::TopLevelWidget

    constructor { args } {
	eval itk_initialize $args
    }

    destructor {
	catch { close }
    }

    # init is called by the TopLevelWidget after the itk options
    # have been evaluated.

    protected method init { } {
	wm protocol $w_ WM_DELETE_WINDOW [code $this close]
	wm title    $w_ "Pick Object"

	config -samplesize 40  ; # sample size at start

	if { [cget -with_menu] } {
	    make_menu ; # Make the menu
	}
	make_layout ; # Make the window layout

	$itk_component(smaller) config -command [code $this inc_zoom -1]
	$itk_component(larger)  config -command [code $this inc_zoom 1]

	# set canvas and widget bindings
	$canvas_ bind all     <ButtonRelease-1> "+[code $this picked_object]"
	$canvas_ bind $image_ <Enter>           "+[code $this config_cursor]"

	bind enter$w_ <Enter> "+[code $this this_entered 1]"
	bind leave$w_ <Leave> "+[code $this this_entered 0]"
        bindtags $w_ "enter$w_ leave$w_"
	
	wm resizable $w_ 0 0  ; # resizing is possible but makes layout worse
    }

    # Make the menu. Add menubar and menus

    protected method make_menu { } {
	add_menubar
	set m [add_menubutton File]
	add_menuitem $m command "Close" \
		{Close this window} \
		-command [code $this close]

	set m [add_menubutton View]
	add_menuitem $m checkbutton "Update after image events" \
		{Update statistics after every new real-time image event} \
		-variable $w_.postImageEvent -onvalue 1 -offvalue 0 \
		-command [code $this setPostImageEvent]

	global ::$w_.postImageEvent
	set $w_.postImageEvent [cget -postImageEvent]

	set magMenu_ $m.mag
	add_menuitem $m cascade "Magnification" \
		{Set the magnification factor of the zoom} \
		-menu [menu $magMenu_]

	loop i 2 31 {
	    $m.mag add radiobutton -label " ${i}x" \
		    -command [code $this scaleZoom $i] \
		    -variable $magMenu_
	}
    }

    # Make the window layout

    protected method make_layout { } {
	if { [cget -with_menu] } {
	    set w $w_.mainf
	    # use same geometry manager as for the menubar
	    pack [frame $w] -expand 1 -fill both
	} else {
	    set w $w_
	}
	if { "[cget -panel_orient]" == "horizontal" } {
	    add_mframe $w infof   make_info     [cget -with_info] raised  0,0 "-fill both"
	    add_mframe $w zoomf   make_zoom     1 raised  0,1 "-fill both"
	    add_mframe $w sliderf make_sldframe [cget -with_slider]  raised  1,0 "-fill both"
	    add_mframe $w choicef make_choice   [cget -with_choice] raised  1,1 "-fill both"
	    add_mframe $w buttonf make_buttons  [cget -with_buttons] groove  2,0 \
		    "-columnspan 2 -fill x"
	} else {
	    add_mframe $w infof   make_info     [cget -with_info] raised  2,0 "-fill both"
	    add_mframe $w zoomf   make_zoom     1 raised  0,0 "-fill both"
	    add_mframe $w sliderf make_sldframe [cget -with_slider] raised  3,0 "-fill both"
	    add_mframe $w choicef make_choice   [cget -with_choice] raised  1,0 "-fill both"
	    add_mframe $w buttonf make_buttons  [cget -with_buttons] groove  4,0 "-fill x"
	}
    }

    # Add a frame to the main blt table of this mega widget.
    # Add frame component, execute the method which create the widgets
    # within the component and add the component to the blt table.

    protected method add_mframe { w compo methd map relief idx opts } {
	itk_component add $compo {
	    frame $w.$compo -borderwidth 2 -relief $relief
	}
	eval $methd $itk_component($compo)
	if { $map } {
	    eval blt::table $w $itk_component($compo) $idx $opts
	}
    }

    # Add short help text for a component

    protected method add_shelp { compo text } {
	add_short_help $itk_component($compo) $text
    }

    # Add a LabelValue widget to a blt table

    protected method add_label { w compo text idx { font labelfont } } {
	set wdg $w.$compo
	itk_component add $compo {
	    util::LabelValue $wdg \
		    -text        $text \
		    -labelfont   $itk_option(-$font) \
		    -labelwidth  $itk_option(-labelwidth) \
		    -valuewidth  $itk_option(-valuewidth) \
		    -relief      groove \
		    -anchor      e
	}
	catch {
	    [$wdg component entry] config -justify right -highlightthickness 0
	    [$wdg component label] config -pady 0
	}
	blt::table $w $wdg $idx -fill x -anchor e
    }

    # Create the widgets for displaying the statistics

    protected method make_info { w } {
	set idx -1
	add_label $w  x      "Image X:"          [incr idx],0
	add_label $w  y      "Image Y:"          [incr idx],0
	add_label $w  ra     "a:"                [incr idx],0  wcsfont
	add_label $w  dec    "d:"                [incr idx],0  wcsfont
	add_label $w  equin  "Equinox:"          [incr idx],0
	add_label $w  peak   "Peak above bg:"    [incr idx],0
	add_label $w  back   "Background level:" [incr idx],0
	add_label $w  fwhm   "FWHM X:Y:"         [incr idx],0
	add_label $w  angle  "Angle of X axis:"  [incr idx],0
	add_label $w  nsize  "Pixels in x,y:"    [incr idx],0

	add_shelp x      "X image pixel coordinate (or X detector chip coord if known)"
	add_shelp y      "Y Image pixel coordinate (or Y detector chip coord if known)"
	add_shelp ra     "World Coordinates RA value"
	add_shelp dec    "World Coordinates DEC value"
	add_shelp equin  "World Coordinates equinox (default: J2000)"
	add_shelp peak   "Object: peak value of object above background"
	add_shelp back   "Background: mean background level"
	add_shelp fwhm   "FWHM: full width half maximum in X and Y"
	add_shelp angle  "Angle: angle of major axis, degrees, along X = 0"
	add_shelp nsize  "Number of pixels: along x and y-axis "
    }

    # Create a zoom window used to display the part of the
    # image to be examined.

    protected method make_zoom { w } {
	itk_component add zoomView {
	    rtd::RtdImagePickView $w.zoomView \
		    -target_image  $itk_option(-target_image) \
		    -factor        $itk_option(-zoomFact) \
		    -command       [code $this zoomScaledCb]
	}
	blt::table $w $itk_component(zoomView) 0,0
    }

    # Add a button and add to blt table

    protected method add_button { w compo text idx command } {
	itk_component add $compo {
	    button $w.$compo -text $text -command $command
	}
	blt::table $w $itk_component($compo) $idx -fill x
    }

    # Add an application specific button

    public method add_my_button { idx compo text command shelp } {
	set w $itk_component(buttonf)

	if { "[cget -panel_orient]" == "horizontal" } {
	    # add button and redo packing
	    set names [blt::table search $w -pattern *]
	    add_button $w $compo $text 0,$idx $command
	    
	    set names [linsert $names $idx $itk_component($compo)]
	    set i -1
	    foreach name $names {
		blt::table $w $name 0,[incr i] -fill x
	    }
	} else {
	    add_button $w $compo $text 1,$idx $command
	}
	add_shelp $compo $shelp
	return $itk_component($compo)
    }

    # Add control buttons

    protected method make_buttons { w } {
	set idx -1

	add_button $w pick   "Pick Object" 0,[incr idx]  [code $this pick_object]
	add_button $w cancel "Cancel"      0,[incr idx]  [code $this cancel 1]
	add_button $w close  "Close"       0,[incr idx]  [code $this close]

	add_shelp pick \
		{Pick Object: {bitmap b1} = select object in image and display \
		center and other statistics}
	add_shelp cancel {Cancel: cancel the current pick operation}
	add_shelp close  {Close: close the pick window}
    }

    # Create the slider widget

    protected method make_slider { w } {

	itk_component add slider {
	    util::LabelEntryScale $w.slider \
		    -show_arrows   1 -increment 1 -valuewidth 4 \
		    -text          "Sample size (in image pixels):"  \
		    -orient        vertical \
		    -length        $itk_option(-maxSsize)  \
		    -value         [cget -samplesize] \
		    -from          $itk_option(-minSsize)  \
		    -to            $itk_option(-maxSsize) \
		    -validate      numeric  \
		    -scaleWidth    10
	}
	add_shelp slider \
		{Slider: set the size of the image area to examine (in image pixels)}

	# configure slider widget and change the layout
	set sldWdg $itk_component(slider)
	$sldWdg config -command [code $this update_rect] \
		-entrycommand [code $this update_rect]
	[$sldWdg component label] config -justify left

	blt::table $sldWdg \
		[$sldWdg component label]      0,0 -anchor w -fill x \
		[$sldWdg component entry]      0,1 -anchor e \
		[$sldWdg component scaleframe] 1,0 -fill x -columnspan 2 -padx 1m
    }

    # add slider widget

    protected method make_sldframe { w } {
	make_slider $w
	blt::table $w $itk_component(slider) 0,0 -anchor w -fill x
    }

    # Create pick control frame

    protected method make_choice { w } {
	# pick mode choice
	set rbChoice $w.rbChoice
	itk_component add rbChoice {
	    util::LabelChoice $rbChoice \
		    -text       "Select pick mode" \
		    -orient     vertical \
		    -choice     {"Pick Object" "Pick Cursor"} \
		    -variable   $w_.pickMode \
		    -value      "Pick Object" \
		    -anchor     c \
		    -command    [code $this set_pickmode]
	}

	# cross marker checkbutton and zoom buttons
	set zoomctrlf $w.zoomctrlf
	itk_component add zoomctrlf { frame $zoomctrlf }
	
	# button to show cross markers
	set var $w_.pickMark
	global ::$var
	itk_component add pickMark {
	    checkbutton $zoomctrlf.pickMark -text "X" -variable $var \
		    -command [code $this toggleMarker $var]
	}
	set $var $itk_option(-showMarker)
	add_shelp pickMark {Show/hide cross marker when object was picked}

	# Zoom buttons
	itk_component add larger {
	    button $zoomctrlf.larger -bitmap magnify -command [code $this inc_zoom 1]   
	}
	itk_component add smaller {
	    button $zoomctrlf.smaller -bitmap shrink -command [code $this inc_zoom -1]  
	}
	add_shelp larger  {Zoom larger:  {bitmap b1} = increase magnification of zoom image}
	add_shelp smaller {Zoom smaller: {bitmap b1} = decrease magnification of zoom image}

	# label for scale factor
	itk_component add scalelab {
	    label $zoomctrlf.label -text "" -width 3 -font $itk_option(-labelfont) 
	}

	blt::table $zoomctrlf \
		$itk_component(pickMark)  0,0  -fill y -anchor w -columnspan 3 \
		$itk_component(larger)    1,0  -fill x \
		$itk_component(smaller)   1,1  -fill x \
		$itk_component(scalelab)  1,2  -fill x

	blt::table $w \
		$itk_component(rbChoice)  0,0 -fill both \
		$itk_component(zoomctrlf) 0,1 -fill both
    }

    # activate / de-activate the widget. 

    protected method activate { bool } {
	if { $bool == $activated_ || ( $bool != 0 && [$image_ isclear] ) } { return }
	
	set im $itk_option(-target_image)
	if { $bool } {
	    # use the "new image" callback for initialization
	    newImageCb

	    # expand zoom window to fill the zoom frame (minus the borderwidth)
	    set bd 6
	    set width  [expr {[winfo width  $itk_component(zoomf)] - $bd}]
	    set height [expr {[winfo height $itk_component(zoomf)] - $bd}]
	    $itk_component(zoomView) change_size $width $height

	    # activate the zoom view widget
	    $itk_component(zoomView) activate 1

	    # install callbacks for new images and image events
	    set cmd [$im cget -newimagecmd]
	    set newImgCmd_ " ; [code $this newImageCb]"
	    $im config -newimagecmd "$cmd$newImgCmd_"
	    
	    set cmd [$im cget -cameraPostCmd]
	    set postCmd_   " ; [code $this updateImageCb]"
	    $im config -cameraPostCmd "$cmd$postCmd_"
	} else {
	    # de-activate the zoom view widget
	    $itk_component(zoomView) activate 0

	    # remove callbacks for new images and image events
	    set cmd [$im cget -newimagecmd]
	    regsub $newImgCmd_ $cmd "" cmd
	    $im config -newimagecmd $cmd

	    set cmd [$im cget -cameraPostCmd]
	    regsub $postCmd_ $cmd "" cmd
	    $im config -cameraPostCmd $cmd
	}
	set activated_ $bool
    }

    # newImageCb is called whenever a new image (i.e. an image with
    # a different type or size) is loaded and also when it is cleared.

    protected method newImageCb { args } {
	if { [$image_ isclear] } {
	    cancel
	    if { "$itk_option(-newImageCmd)" != ""} {
		eval $itk_option(-newImageCmd)
	    }
	    return
	}
	updateMarker 0

	# check that the last picked coords are within the range
	# of the new image. If not, use the center coords.
	set width  [$image_ width]
	set height [$image_ height]

	# check if there was one pick
	if {"$pickx_" == ""} {
	    $image_ convert coords \
		[expr {$width / 2.0}] [expr {$height / 2.0}] image \
		    pickx_ picky_ chip
	}
	$image_ convert coords $pickx_ $picky_ chip x y image

	# check if center coords are in bounds
	if { $x > $width || $y > $height } {
	    $image_ convert coords $x $y image pickx_ picky_ chip
	    set_values {}
	}
	$itk_component(zoomView) moveTo $pickx_ $picky_

	# check sample size
	set size [cget -samplesize]
	set maxsize [min $width $height]
	if { $size > $maxsize } {
	    set size $maxsize
	    config -samplesize $size
	}
	$itk_component(slider) config -value $size
	$itk_component(zoomView) config -ssize $size

	if { "$itk_option(-newImageCmd)" != ""} {
	    eval $itk_option(-newImageCmd)
	}
    }

    # updateImageCb is called after an image event

    protected method updateImageCb { args } {
	catch {
	    # check if the statistics of the actual image can be updated
	    if { ! [cget -postImageEvent] || $picking_ || ! $activated_ || \
		    $pickMode_ == 1 || [ $image_ isclear ] } {
		return
	    }
	    if { [lempty $pickx_] } { return }
	    set result [$itk_component(zoomView) statistics $pickx_ $picky_]
	    $itk_component(zoomView) moveTo $pickx_ $picky_
	    set_values $result 1
	}
    }

    # setup new center coords and sample size for method pick_object

    public method setup_pick { xref yref sampleSize } {
	update_rect $sampleSize
	set_values "$xref $yref"
	set pickx_ $xref
	set picky_ $yref
	$itk_component(zoomView) moveTo $xref $yref
    }

    # pick an object in the image and get the statistics on
    # the area. Currently the args list is not used.

    public method pick_object { args } {
	if { $picking_ || [$image_ isclear] } { return }

	# update and raise is needed for method activate
	raise $w_
	update idletasks
	updateMarker 0

	# prepare for <ButtonRelease-1> event which calls method picked_object
	activate 1 ; # activate zoom
	picking  1 ; # change mode to "picking"
    }

    # picked_object is called when the user has clicked in the image 
    # to select an object or star for the "pick_object" method. 

    protected method picked_object { } {
	# return if the mouse click is not for us
	set cs [lindex "$cs_pick_ $cs_click_" $pickMode_]
	if { ! $picking_ || "$cs" != "[$canvas_ cget -cursor]" } { return }

	# get chip coordinates at cursor position set by the rtdimage code
	global ::$image_
	lassign "[set ${image_}(X)] [set ${image_}(Y)]" x y

	picking 0 ; # stop the zoom

	if { [$image_ isclear] } { return }

	# set result according to the current pick mode
	if { $pickMode_ == 0 } {
	    # get statistics
	    $itk_option(-target_image) busy {
		set result [$itk_component(zoomView) statistics $x $y]
	    }
	} else {
	    # set result to the displayed x,y coords
	    set result "$x $y"
	}
	lassign $result xc yc
	set pickx_ $xc
	set picky_ $yc

	# move to new center of image
	$itk_component(zoomView) moveTo $xc $yc
	raise $w_
	set_values $result

	if { "$itk_option(-pickedCmd)" != "" } {
	    eval $itk_option(-pickedCmd) {"[list $result [cget -samplesize]]"}
	}
    }

    # close this window

    protected method close { } {
	cancel 1   ; # cleanup
	activate 0 ; # de-activate the zoom and events
	wm withdraw $w_
    }

    # set pick mode

    protected method set_pickmode { args } {
	global ::$w_.pickMode
	if { "[set $w_.pickMode]" == "Pick Object" } {
	    set pickMode_ 0
	} else {
	    set pickMode_ 1
	}
	$itk_component(pick) config -text \
		[lindex [$itk_component(rbChoice) cget -choice] $pickMode_]
	config_cursor
    }

    # cancel pick operation

    public method cancel { { eval_cmd 0 } } {
	if { $eval_cmd } {
	    updateMarker 0
	    if { "$itk_option(-cancelCmd)" != ""} {
		eval $itk_option(-cancelCmd)
	    }
	}
	picking 0
    }

    # Show image at last picked coords when mouse ptr. enters this widget.
    # Otherwise: switch zoom on when picking is active, else off.

    public method this_entered { bool } {
	if { ! $bool && $picking_ } {
	    $itk_component(zoomView) zoom 1
	} else {
	    $itk_component(zoomView) zoom 0
	    if { [lempty $pickx_] } { return }
	    $itk_component(zoomView) moveTo $pickx_ $picky_
	}
    }

    # change cursor when mouse pointer enters or leaves the image.
    # This is only done when the default cursor is displayed.

    protected method config_cursor { } {
	set cursor [$canvas_ cget -cursor]
	set cs [lindex "$cs_pick_ $cs_click_" $pickMode_]
	if { $picking_ } {
	    if { "$cursor" == "" || "$cursor" == "$cs_pick_" || \
		    "$cursor" == "$cs_click_"} {
		$canvas_ configure -cursor $cs
		$w_ configure -cursor $cs
	    }
	} else {
	    if { "$cursor" == "$cs_pick_" || "$cursor" == "$cs_click_"} {
		$canvas_ configure -cursor {}
		$w_ configure -cursor {}
	    }
	}
    }

    # set state picking / zooming and configure the widgets

    protected method picking { bool } {
	if { $bool == $picking_ } { return }

	if { $bool } {
	    $itk_component(zoomView) zoom 1
	    $itk_component(pick) config -state disabled
	} else {
	    $itk_component(zoomView) zoom 0
	    $itk_component(pick) config -state normal
	}
	set picking_ $bool
	config_cursor
    }

    # set the values of the labels from the list (results of "pick_object" call).
    # If list is empty the labels are cleared.

    protected method set_values { list { imgEvt 0 } } {
	set list_ ""
	foreach wdg [winfo children $itk_component(infof)] {
	    catch { $wdg config -value "" }
	}
	if { [lempty $list] } {
	    return
	}
	set pickOk 1
	lassign $list x y ra dec equin fwhmX fwhmY angle peak back

	$itk_component(x)     config -value  [format_val $x]
	$itk_component(y)     config -value  [format_val $y]
	$itk_component(nsize) config -value  [format_val [cget -samplesize]]

	if { $pickMode_ == 0 } {
	    if {"$fwhmX" == "" || $fwhmX > 0 && $fwhmY > 0} {
		$itk_component(ra)    config -value  $ra
		$itk_component(dec)   config -value  $dec
		$itk_component(equin) config -value  $equin
		$itk_component(peak)  config -value  [format_val $peak]
		$itk_component(back)  config -value  [format_val $back]
		$itk_component(angle) config -value  [format_val $angle]
		$itk_component(fwhm)  config -value "[format_val $fwhmX] : [format_val $fwhmY]"
		set fgcol black
	    } else {
		# "Can't do" was displayed by the first astronomical image processing
		# system in the world, named IHAP.
		if { ! $imgEvt } {
		    $itk_component(fwhm)  config -value "Can't do"
		}
		$itk_component(angle) config -value ""
		set fgcol red
		set pickOk 0
	    }
	    [$itk_component(fwhm) component entry] config -foreground $fgcol
	}
	lappend list_ $list "$pickMode_ $pickOk"
	updateMarker
    }

    # update the size of the square in the zoom canvas from the slider widget.
    # "size" is the size of the sample image in image pixels

    public method update_rect { {size 0} }  {
	# check requested size
	if {$size < $itk_option(-minSsize) || \
		$size > $itk_option(-maxSsize) || \
		$size > [min [$image_ width] [$image_ height]]} {
	    $itk_component(slider) select  ; # just select - no warning message
	    return
	}
	config -samplesize $size
	$itk_component(zoomView) config -ssize $size
	$itk_component(zoomView) update_rect
	$itk_component(slider) config -value $size

	if { "$itk_option(-newSizeCmd)" != "" } {
	    eval $itk_option(-newSizeCmd) $size
	}
    }

    # increment or decrement the zoom factor

    protected method inc_zoom { inc } {
	$itk_component(zoomView) inc_zoom $inc
	updateMarker
    }

    # display the scale factor of the zoom widget

    protected method zoomScaledCb { args } {
	set f [$itk_component(zoomView) get_scale]
	$itk_component(scalelab) config -text "${f}x"
    }

    # scale the zoom widget

    protected method scaleZoom { scale } {
	$itk_component(zoomView) set_scale $scale
	zoomScaledCb
	updateMarker
    }

    # format a floating point value (which may also be empty)

    protected method format_val { val } {
	if {"$val" == ""} {
	    return
	}
	return [format {%.1f} $val]
    }

    # configure the -postImageEvent option

    protected method setPostImageEvent { args } {
	global ::$w_.postImageEvent
	config -postImageEvent [set $w_.postImageEvent]
    }

    # toggle the visibility of the blinking marker

    protected method toggleMarker { var } {
	global ::$var
	config -showMarker [set $var]
	updateMarker
    }

    # show/hide blinking marker when object was picked. In pick
    # cursor mode a simple cross is displayed and in pick object
    # mode the cross marking the fwhm values.

    protected method updateMarker { {show -1} } {
	if { [lempty $list_] && $show != 0 } { return }

	set zoomIm [$itk_component(zoomView) get_image]
	set zoomCv [$itk_component(zoomView) get_canvas]

	if { $show == -1 } {
	    set show [cget -showMarker]
	}
	lassign [lindex $list_ 0] x y ra dec equinox fwhmX fwhmY angle peak background
	lassign [lindex $list_ 1] pickMode pickOk

	if { $show && $pickOk } {
	    $image_ convert coords $x $y chip x y image
	    if { $pickMode == 0 } {
		mark_spot $x $y $zoomIm $zoomCv  $angle $fwhmX $fwhmY
		mark_spot $x $y $image_ $canvas_ $angle $fwhmX $fwhmY 1
	    } else {
		$image_ convert coords 5 5 image nx ny image
		mark_spot $x $y $zoomIm $zoomCv  0 $nx $ny
		mark_spot $x $y $image_ $canvas_ 0 $nx $ny 1
	    }
	} else {
	    catch {$zoomCv  delete mark$zoomIm}
	    catch {$canvas_ delete mark$image_}
	}
    }

    # mark the x,y image coordinate point in the canvas with
    # a cross with the width, height (image pixels) and angle (deg).

    protected method mark_spot {xc yc image canvas angle w h {blink 0}} {
	set tags "mark$image objects"
	catch { $canvas delete mark$image }

	if { $xc > [$image width] || $yc > [$image height] || \
		 $xc < 1 || $yc < 1 || [catch {expr {$angle / 2.0}}] } { 
	    return
	}

	# convert angle to radian
	set rad [expr {$angle / 57.2958}]

	# deltas for X and Y axis
	set dxX [expr {cos($rad) * $w/2.0}]
	set dyX [expr {sin($rad) * $w/2.0}]
	set dxY [expr {cos($rad) * $h/2.0}]
	set dyY [expr {sin($rad) * $h/2.0}]

	# compute end points for X-axis and convert points to canvas coordinates
	$image convert coords [expr {$xc + $dxX}] [expr {$yc + $dyX}] image x1X y1X canvas
	$image convert coords [expr {$xc - $dxX}] [expr {$yc - $dyX}] image x2X y2X canvas

	# the Y-axis is rotated "by hand" so that it appears perpendicular to the X-axis
	$image convert coords [expr {$xc + $dyY}] [expr {$yc - $dxY}] image x1Y y1Y canvas
	$image convert coords [expr {$xc - $dyY}] [expr {$yc + $dxY}] image x2Y y2Y canvas

	# draw X and Y axis lines with an outer thick black line
	# and inner thin white line
	foreach width {3 1} bg {black white} {
	    set opts "-fill $bg -width $width -tags {$tags $bg}"
	    eval $canvas create line $x1X $y1X $x2X $y2X $opts
	    eval $canvas create line $x1Y $y1Y $x2Y $y2Y $opts
	}
	if { $blink } {
	    blink_mark $canvas $tags
	}
    }

    # blink a cross in the main image, showing the marker created
    # by method mark_spot

    protected method blink_mark { canvas tags { color 0 } } {
	catch {after cancel $afterId_}
	set tag [lindex $tags 0]
	if { "[$canvas gettags $tag]" == "" } { return }

	set cols "black white"
	if { $color } {
	    set cols "white black"
	}
	$canvas itemconfigure white -fill [lindex $cols 0]
	$canvas itemconfigure black -fill [lindex $cols 1]
	set afterId_ [after 700 [code $this blink_mark $canvas $tag [expr {! $color}]]]
    }

    # -- options --

    # target (main) RtdImage itcl widget
    itk_option define -target_image target_image Target_image { } {
        set image_  [[cget -target_image] get_image]
        set canvas_ [[cget -target_image] get_canvas]
    }
    # actual sample size
    itk_option define -samplesize sampleSize SampleSize {}

    # min. and max. values for slider
    itk_option define -minSsize minSsize MinSsize 5
    itk_option define -maxSsize maxSsize MaxSsize 100

    # command to evaluate when a an object was picked
    itk_option define -pickedCmd pickedCmd PickedCmd {}

    # command to evaluate when the sample size changed
    itk_option define -newSizeCmd newSizeCmd NewSizeCmd {}

    # command to evaluate after a new image
    itk_option define -newImageCmd newImageCmd NewImageCmd {}

    # command to evaluate when the pick was canceled
    itk_option define -cancelCmd cancelCmd CancelCmd {}

    # cursors to use
    itk_option define -pick_cursor pick_cursor Pick_cursor {target} {
	set cs_pick_ [cget -pick_cursor]
    }
    itk_option define -click_cursor click_cursor Click_cursor {plus} {
	set cs_click_ [cget -click_cursor]
    }
    
    # update statistics after every image event, bool
    itk_option define -postImageEvent postImageEvent PostImageEvent 1

    # default: show marker (blinking cross) when object was picked, bool
    itk_option define -showMarker showMarker ShowMarker 1

    # default zoom factor
    itk_option define -zoomFact zoomFact ZoomFact 4

    # "show" options: menubar, slider, info, ...
    itk_option define -with_menu with_menu With_menu 0
    itk_option define -with_slider with_slider With_slider 1
    itk_option define -with_info with_info With_info 1
    itk_option define -with_choice with_choice With_choice 1
    itk_option define -with_buttons with_buttons With_buttons 1

    # Specify the orientation of image and panel, one of {vertical horizontal}
    itk_option define -panel_orient panel_orient Panel_orient {horizontal}

    # fonts, widths, etc.
    itk_option define -labelfont  labelFont  LabelFont -Adobe-helvetica-bold-r-normal--12*
    itk_option define -valuefont  valueFont  ValueFont -Adobe-helvetica-medium-r-normal--12*
    itk_option define -wcsfont    wcsFont    WcsFont   -*-symbol-*-*-*-*-14-*-*-*-*-*-*-*
    itk_option define -labelwidth labelWidth LabelWidth 15
    itk_option define -valuewidth valueWidth ValueWidth 11

    # -- protected vars --
    
    protected variable image_        ;# internal target image
    protected variable canvas_       ;# target canvas
    protected variable picking_   0  ;# user is picking object, bool
    protected variable pickMode_  0  ;# pick mode (0=object, 1=cursor)
    protected variable afterId_      ;# id for blink after job
    protected variable list_      {} ;# result of last pick operation
    protected variable activated_ 0  ;# widget activated, bool
    protected variable newImgCmd_    ;# our callback for a new image cmd
    protected variable postCmd_      ;# our callback for a post image cmd
    protected variable magMenu_      ;# widget name of magnification menu
    protected variable cs_pick_      ;# cursor used for picking object
    protected variable cs_click_     ;# cursor used for clicking object
    protected variable pickx_     {} ;# last picked x coord
    protected variable picky_     {} ;# last picked y coord
}
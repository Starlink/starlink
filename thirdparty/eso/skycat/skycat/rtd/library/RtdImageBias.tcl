#******************************************************************************
# E.S.O. - VLT project
# "@(#) $Id: RtdImageBias.tcl,v 1.1.1.1 2006/01/12 16:38:16 abrighto Exp $"
#
# RtdImageBias.tcl - class for bias data subtraction
#
# who          when      what
# --------     --------  ----------------------------------------------
# P.Biereichel 22/03/99  Created

itk::usual RtdImageBias {}

# RtdImageBias is a class for controlling the subtraction of
# a bias frame.

itcl::class rtd::RtdImageBias {
    inherit util::TopLevelWidget 
    
    constructor {args} {
	eval itk_initialize $args
	wm title $w_ "Bias Image"
        wm protocol $w_ WM_DELETE_WINDOW "$this close"
	set biasWdg_ $this
    }
    
    public method init {} {
	set maxbias_ [$image_ biasimage maxbias]
        add_menubar
        make_layout
	update idletasks
	wm minsize $w_ [winfo reqwidth $w_] [winfo reqheight $w_]
	update_cuts
    }
    
    # Method to add the menu bar to the top of the dialogue.
    
    protected method add_menubar {} {
        TopLevelWidget::add_menubar
	
        # File menu
        set m [add_menubutton File]

        add_menuitem $m cascade "Load File" \
            {Load a file for bias subtraction} \
            -menu [menu $m.biasf]

	for {set i 0} {$i < $maxbias_} {incr i} {
	    $m.biasf add command -label "Bias [expr {$i+1}]..." \
		    -command [code $this load $i]
	}

        add_menuitem $m cascade "Clear" \
            {Clear a bias image} \
            -menu [menu $m.biasc]

	for {set i 0} {$i < $maxbias_} {incr i} {
	    $m.biasc add command -label "Bias [expr {$i+1}]" \
		    -command [code $this clear $i]
	}
	$m.biasc add command -label "All" \
		    -command [code $this clear all]

        add_menuitem $m cascade "Copy image to ->" \
            {Copy current image to a bias frame} \
            -menu [menu $m.biaso]

	for {set i 0} {$i < $maxbias_} {incr i} {
	    $m.biaso add command -label "Bias [expr {$i+1}]" \
		    -command [code $this copy $i]
	}

        add_menuitem $m command "Close" \
            {Close this window} \
            -command [code $this close]

        # View menu
        set m [add_menubutton View]

        add_menuitem $m command "Display selected bias image" \
            {Display the bias image} \
            -command [code $this display]
    }


    # make the widget layout

    protected method make_layout {} {
	# frames
	foreach el "bias status cuts buttons" {
	    itk_component add $el {
		frame $w_.$el -relief groove -borderwidth 2
	    }
	    pack $itk_component($el) -fill x -expand 1
	}

	itk_component add copy {
	    button $w_.copy \
		    -text "Copy image -> bias" \
		    -command [code $this copy]
	}

        # checkbutton to turn bias subtraction on/off
        itk_component add onoff {
            checkbutton $w_.onoff \
                -text "Subtract " \
                -variable $w_.onoff \
                -onvalue 1 -offvalue 0 \
                -anchor w \
                -borderwidth 2 -relief raised -pady 3 \
                -command [code $this onOff]
        } { keep -state }

        blt::table $itk_component(bias) \
            $itk_component(onoff)         0,0 -pady 3 \
            $itk_component(copy)          0,1 -pady 3

	set i 0
	foreach el "Nr Select Load Copy Clear Filename" {
	    set comp [string tolower $el]
	    itk_component add label$comp {
		label $w_.label$comp \
			-anchor c
	    }
	    blt::table $itk_component(status) \
		    $itk_component(label$comp)      0,$i
	    blt::table configure $itk_component(status) c$i -resize none
	    incr i
	}
	blt::table configure $itk_component(status) c5 -resize expand

	foreach el "Load Copy Clear" {
	    set s [string tolower $el]
	    catch {bitmap compose $s $el -rotate 90.0 -font $itk_option(-valuefont)}
	    $itk_component(label$s) config -bitmap $s -anchor w
	}

	for {set i 0} {$i < $maxbias_} {incr i} {
	    set n [expr {$i+1}]

	    itk_component add labelnr$i {
		label $w_.labelnr$i \
		    -text " [expr {$i+1}] " \
			-font $itk_option(-labelfont) \
			-anchor c
	    }

	    global ::$w_.select
	    itk_component add select$i {
		radiobutton $w_.select$i \
			-variable $w_.select \
			-anchor c \
			-value $i \
			-padx 1 -pady 1 \
			-borderwidth 2 \
			-command [code $this select]
	    }
	    set select_ [set $w_.select 0]

	    itk_component add choose$i {
		button $w_.choose$i \
			-text "..." \
			-width 2 \
			-padx 0 \
			-anchor c \
			-font $itk_option(-valuefont) \
			-borderwidth 2 \
			-relief raised \
			-command [code $this load $i]
	    }

	    itk_component add copy$i {
		button $w_.copy$i \
			-text "->" \
			-width 2 \
			-anchor c \
			-padx 0 \
			-font $itk_option(-valuefont) \
			-borderwidth 2 \
			-relief raised \
			-command [code $this copy $i]
	    }

	    itk_component add clear$i {
		button $w_.clear$i \
			-text "C" \
			-anchor c \
			-width 2 \
			-padx 0 \
			-font $itk_option(-valuefont) \
			-borderwidth 2 \
			-relief raised \
			-command [code $this clear $i]
	    }

	    itk_component add filename$i {
		util::LabelValue $w_.filename$i \
			-text "" \
			-labelwidth 2 \
			-valuefont $itk_option(-valuefont) \
			-labelfont $itk_option(-labelfont) \
			-valuewidth 30 \
			-anchor e \
			-relief groove \
			-orient horizontal
	    }

	    blt::table $itk_component(status) \
		    $itk_component(labelnr$i)     $n,0 \
		    $itk_component(select$i)      $n,1 \
		    $itk_component(choose$i)      $n,2 \
		    $itk_component(copy$i)        $n,3 \
		    $itk_component(clear$i)       $n,4 \
		    $itk_component(filename$i)    $n,5 -anchor e -fill x -pady 1
	}

	itk_component add labelcut {
	    label $w_.labelcut \
		    -text "Cut levels:" \
		    -font $itk_option(-labelfont) \
		    -anchor c
	}

	foreach el "Low High" {
	    set s [string tolower $el]
	    itk_component add $s {
		LabelEntry $w_.$s \
			-text "$el:" \
			-command [code $this set_cut_levels] \
			-labelfont $itk_option(-labelfont) \
			-valuefont $itk_option(-valuefont) \
			-labelwidth $itk_option(-labelwidth) \
			-valuewidth $itk_option(-valuewidth) \
			-anchor e \
			-relief sunken \
			-validate real
	    } { keep -state }
	}

	itk_component add autocut {
	    button $w_.autocut \
                    -text "Auto Set Cut Levels" \
                    -font $itk_option(-labelfont) \
                    -command [code $this auto_set_cut_levels]       
	}

        blt::table $itk_component(cuts) \
		$itk_component(labelcut)     0,0 -columnspan 3 -pady 4 \
		$itk_component(low)          1,0 -anchor w -ipady 3 -pady 2 \
		$itk_component(high)         1,1 -anchor w -ipady 3 -pady 2 \
		$itk_component(autocut)      1,2 -anchor w -ipady 3 -pady 2


	itk_component add close {
	    button $itk_component(buttons).close \
		    -text Close \
		    -command [code $this close]
	}

        blt::table $itk_component(buttons) \
            $itk_component(close)        0,0 -pady 3

    }
    
    # add short help text

    public method make_short_help {} {
        add_help $itk_component(copy) "Copy: Copy current image to selected bias image"
        add_help $itk_component(onoff) "Subtract: Switch subtraction of bias image on/off"
	for {set i 0} {$i < $maxbias_} {incr i} {
	    set n [expr {$i+1}]
	    add_help $itk_component(labelnr$i) "Number of bias image"
	    add_help $itk_component(select$i) "Select: Select bias image $n for image subtraction"
	    add_help $itk_component(choose$i) "...: Load bias image $n from file"
	    add_help $itk_component(copy$i) "->: Copy current image -> bias image $n"
	    add_help $itk_component(clear$i) "C: Clear bias image $n"
	    add_help $itk_component(filename$i) "File: Filename of bias image $n or env. RTD_CAMERA \
		    for real-time image"
	}
        add_help $itk_component(low) "Low: Image low cut level, hit Return after editing value"
        add_help $itk_component(high) "High: Image high cut level, hit Return after editing value"
        add_help $itk_component(autocut) "Autocut: Auto set cut levels"
        add_help $itk_component(close) "Close: Close this window"
    }

    protected method add_help {compo msg} {
	# XXX would be nice to have an option for TopLevelWidget
	bind $compo <Enter> {}
	bind $compo <Leave> {}
	add_short_help $compo $msg
    }

    # close this window

    public method close {} {
	wm withdraw $w_
    }

    # trace status of a global variable which is set by the rtdimage
    # subcommand 'biasimage'

    protected method trace_status {} {
        global ::$image_
	trace variable ${image_}(BIAS) w [code $this updateStatus]
    }

    # copy current image -> bias image

    public method copy {{nr -1}} {
	if [$image_ isclear] {
	    warning_dialog "No image is currently loaded" $w_
	    return
	}
	if {$nr == -1} {
	    set nr $select_
	}
	if {$nr == $select_ && $onoff_} { set update_ 1 }
	if {[catch {$image_ biasimage copy $nr} msg]} {
	    error_dialog $msg
	}
    }

    # clear bias image

    public method clear {nr} {
	if {$nr == $select_ && $onoff_} { set update_ 1 }
	$image_ biasimage clear $nr
    }

    # select bias image for image subtraction

    public method select {} {
        global ::$w_.select
	if {$select_ == [set $w_.select]} { return }
	set select_ [set $w_.select]
	set update_ $onoff_
	$image_ biasimage select $select_
    }

    # open and load a new FITS image file via file name dialog

    public method load {nr {dir "."} {pattern "*.*fit*"}} {
        set file [filename_dialog $dir $pattern $w_]
        if {"$file" != ""} {
            if {[file isfile $file]} {
		if {$nr == $select_ && $onoff_} { set update_ 1 }
		if {[catch {$image_ biasimage file $file $nr} msg]} {
		    error_dialog $msg
		}
            } else {
                error_dialog "There is no file named '$file'" $w_
            }
        }
    }

    # switch bias subtraction on/off

    public method onOff {} {
	global ::$w_.onoff
	set onoff_ [set $w_.onoff]
	set update_ 1
	if {$onoff_} {
	    if {[catch {$image_ biasimage on} msg]} {
		set onoff_ [set $w_.onoff 0]
		error_dialog $msg
	    }
	} else {
	    $image_ biasimage off
	}
    }

    # display selected bias image

    public method display {} {
	if {[catch {$image_ biasimage display} msg]} {
	    error_dialog $msg
	}
    }

    # update status of this widget and all images when necessary

    public method updateStatus {{args}} {
	global ::$image_ ::$w_.onoff

	if {[$image_ biasimage status] == -1} {
	    set onoff_ [set $w_.onoff 0]
	    config -state disabled
	} else {
	    config -state normal
	}

	set isclear [$image_ isclear]
	set stat normal
	if {$isclear} {
	    set stat disabled
	}
	foreach el "copy low high" {
	    $itk_component($el) config -state $stat
	}

	for {set i 0} {$i < $maxbias_} {incr i} {
	    set fn ""
	    set fn [file tail [$image_ biasimage file $i]]
	    # set fn [$image_ biasimage file $i]
	    $itk_component(filename$i) config -value $fn
	    if {"$fn" != ""} {
		$itk_component(filename$i) config -relief sunken
		$itk_component(clear$i) config -state normal
	    } else {
		$itk_component(filename$i) config -relief groove
		$itk_component(clear$i) config -state disabled
	    }
	    $itk_component(copy$i) config -state $stat
	}

	# update all images when requested
	if {$update_} {
	    foreach el $images_ {
		$el update
	    }
	    set update_ 0
	}
    }
    
    # set the cut levels when the user types them in and hits return
    
    public method set_cut_levels {args} {
	set low [$itk_component(low) get] 
	set high [$itk_component(high) get]
	if {[catch {expr {$low}} msg] || [catch {expr {$high}} msg]} {
	    error_dialog $msg
	} else {
	    $image_ cut $low $high
	    if {"$itk_option(-command)" != ""} {
		eval $itk_option(-command)
	    }
	}
    }

    # set the cut levels automatically using median filtering...
    
    public method auto_set_cut_levels {} {
	busy {$image_ autocut}
	if {"$itk_option(-command)" != ""} {
	    eval $itk_option(-command)
	}
    }

    # update cut level display

    public method update_cuts {} {
	lassign [$image_ cut] low high
	entry_value low $low
 	entry_value high $high
   }

    # write value into entry field

    protected method entry_value {compo value} {
	set w [$itk_component($compo) component entry]
        $w delete 0 end
        $w insert 0 $value
    }
    
    # add an image object whose bias subtraction is managed
    # by this class

    public proc add_image {Img} {
	set img [$Img get_image]
	lvarpush images_ $img
	global ::$img
	set ${img}(BIAS) 0
    }
    
    # remove an image object whose bias subtraction is managed
    # by this class

    public proc remove_image {Img} {
	set img [$Img get_image]
	if {[set idx [lsearch $images_ $img]] == -1} { return }
	set images_ [lreplace $images_ $idx $idx]
	set image_ [lindex $images_ 0]

	global ::$img
	catch {unset ${img}(BIAS) 0}	

	if {[set idx [lsearch $Images_ $Img]] != -1} {
	    set Images_ [lreplace $Images_ $idx $idx]
	}
	set Img [lindex $Images_ 0]
	if {"$Img" != ""} {
	    $biasWdg_ config -shorthelpwin [[$Img cget -shorthelpwin] component hull] -image $Img
	}
    }

    # called after configuring itk_option(-image)

    public method newTarget {} {
	make_short_help
	update_cuts
	updateStatus
    }

    # -- options --
    
    # target widget
    itk_option define -image image Image {} {
	set Img $itk_option(-image)
	set image_ [$Img get_image]

	if {[set idx [lsearch $Images_ $Img]] == -1} {
	    lvarpush Images_ $Img
	    set newimg [$Img cget -newimagecmd]
	    # install callback for new image events
	    $Img config -newimagecmd "$newimg; [code $this updateStatus]"
	    trace_status
	} else {
	    set Images_ [lreplace $Images_ $idx $idx]
	    lvarpush Images_ $Img
	}
	after idle [code $this newTarget]
    }

    # font used for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*

    # font used for values
    itk_option define -valuefont valueFont ValueFont -Adobe-helvetica-medium-r-normal-*-12*

    # set the width for  displaying labels and values
    itk_option define -labelwidth labelWidth LabelWidth 4
    itk_option define -valuewidth valueWidth ValueWidth 8

    # set the state to normal/disabled to enable/disable editing
    itk_option define -state state State {disabled}

    # tcl command to evaluate when cut levels were changed
    itk_option define -command command Command {}

    # display number of main image
    itk_option define -dsplnr dsplnr Dsplnr {1} {
	set nr $itk_option(-dsplnr)
	if {$nr > 1} {
	    set mains_ $nr
	}
	if {$mains_} {
	    wm title $w_ "Bias Image ($nr)"
	}
    }

    # -- protected vars -- 

    # max. numbers of bias frames
    protected variable maxbias_

    # copy of ::$w_.select
    protected variable select_

    # copy of ::$w_.onoff
    protected variable onoff_ 0

    # flag for image update request
    protected variable update_ 0

    # set when more than one instances of main windows are used
    protected variable mains_ 0

    # -- common (shared) variables -- 
    
    # name of current rtd image
    protected common image_ {}

    # list of RtdImage objects handled by this class
    protected common images_ {}
    protected common Images_ {}

    # name of this widget
    protected common biasWdg_ {}
}

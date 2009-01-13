# E.S.O. - VLT project/ESO Archive
# "@(#) $Id: EntryForm.tcl,v 1.1.1.1 2006/01/12 16:40:54 abrighto Exp $"
#
# EntryForm.tcl - Form dialog for entering data at given labels
#
# who           when       what
# --------     ---------   ----------------------------------------------
# A.Brighton   26 Jun 96   created
# P.Biereichel 04/08/99    Added option -scroll
# P.W.Draper   03/12/08    Scroll for width as well as height. Some
#                          reorganisations to make it work.

itk::usual EntryForm {}


# An EntryForm widget is a dialog for entering data at given labels.

itcl::class util::EntryForm {
    inherit util::TopLevelWidget


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }


    # called after options have been evaluated

    protected method init {} {
	# Title for window.
	itk_component add title {
	    label $w_.title -text $itk_option(-title)
	}
	pack $itk_component(title) -side top -fill x -pady 2m -padx 2m

	# Frame containing dialog buttons.
	itk_component add buttons {
	    util::ButtonFrame $w_.buttons \
		-borderwidth 2 \
		-relief raised \
		-ok_label Enter \
		-ok_cmd [code $this enter_data] \
		-cancel_label Close \
		-cancel_cmd [code $this cancel]
	}
	$itk_component(buttons) append Reset [code $this reset]

	foreach i $itk_option(-buttons) {
	    lassign $i label cmd
	    $itk_component(buttons) append $label [code $this eval_cmd $cmd]
	}

	pack $itk_component(buttons) -side bottom -fill x

	if {$itk_option(-scroll)} {
	    # Canvas used to add a scrollbar
	    itk_component add canvas {
		CanvasWidget $w_.canvas
	    }
	    set canvas [$w_.canvas component canvas]
	    pack $w_.canvas -side top -fill both -expand 1 \
		    -padx 1m -pady 1m -ipadx 1m -ipady 1m

	    # Frame containing entries.
	    itk_component add entries {
		set f [frame $canvas.entries -bd 3 -relief groove]
	    }

	    $canvas create window 0 0 -window $f -anchor nw -tags frame
	    $canvas configure -background [$f cget -background]
	    after idle [code $this add_bindings_ $canvas $f]
	} else {
	    itk_component add entries {
		set f [frame $w_.entries -bd 3 -relief groove]
	    }
	    pack $f -side top -fill both -expand 1 \
		    -padx 1m -pady 1m -ipadx 1m -ipady 1m
	}

	blt::table $f
	set row 0
	foreach label $itk_option(-labels) {
	    blt::table $f \
		[label $f.label$row -text $label] \
		$row,0 -anchor e \
		[set entries_($label) [entry $f.entry$row -relief sunken]] \
		$row,1 -fill x -anchor w
	    $f.entry$row insert end [lindex $itk_option(-values) $row]

	    bind $f.entry$row <Return> [code $this enter_data]

	    incr row
	}
	blt::table configure $f c1 -resize expand
        blt::table configure $f c0 -resize none
        blt::table configure $f r* -resize none

	wm title $w_ {Entry Form}
	set initialized_ 1
    }
   
    #  Add bindings to the canvas. Deferred until after window is realised.
    protected method add_bindings_ {canvas f} {
       bind $canvas <Configure> [code $this resize $f $canvas %w %h]
    }

    # Called when the window is resized. the arguments are the
    # entries frame (in the canvas), the canvas and the canvas
    # width and hight.

    public method resize {frame canvas cw ch} {

        #  Don't resize when already resizing.
        if { $resizing_ } return

        set fh [max $ch [winfo height $frame]]
        set fw [max $cw [winfo width $frame]]

        set resizing_ 1
        $canvas configure -width $fw
        $canvas configure -height $fh

        $canvas itemconfigure frame -width $fw -height $fh
	$canvas configure -scrollregion "0 0 $fw $fh"

        #  Make sure we're completed before releasing the lock.
        update
        set resizing_ 0
    }


    # reset to the original values

    public method reset {} {
	set row 0
	foreach i $itk_option(-values) {
	    set w [component entries].entry$row
	    $w delete 0 end
	    $w insert end $i
	    incr row
	}
    }


    # called for the Cancel button

    public method cancel {} {
	destroy $w_
    }


    # set (reset) the value for the given entry

    public method set_entry {label value} {
	if {[info exists entries_($label)]} {
	    $entries_($label) delete 0 end
	    $entries_($label) insert end $value
	}
    }


    # this method is called to enter the data when the Enter button is
    # pressed. Call the command specified by the -command option with
    # a list of values, one for each label.

    protected method enter_data {} {
	eval_cmd $itk_option(-command)
	#destroy $w_
    }


    # add the contents of this window as a list argument to the given
    # command and then evaluate it

    protected method eval_cmd {cmd} {
	if {"$cmd" != ""} {
	    set n 0
	    set list {}
	    set row 0
	    set f $itk_component(entries)
	    foreach label $itk_option(-labels) {
		set s [$f.entry$row get]
		lappend list $s
		incr row
		incr n [string length $s]
	    }
	    if {$n == 0} {
		error_dialog "No data was entered"
		return
	    }
	    lappend cmd $list
	    eval $cmd
	}
    }


    # -- options --

    # title for dialog
    itk_option define -title title Title {}

    # list of labels, one for each entry to be displayed
    itk_option define -labels labels Labels {}

    # optional list of values, one for each entry to be displayed
    itk_option define -values values Values {} {
	if {$initialized_} {
	    reset
	}
    }

    # called with list of values when Enter button is pushed
    itk_option define -command command Command {}

    # list {{label cmd} {label cmd}} of additional buttons to display
    itk_option define -buttons buttons Buttons {}

    # use scrollbar option
    itk_option define -scroll scroll Scroll {1}


    # -- protected vars --

    # array(label) of entry widget for given label
    protected variable entries_

    # flag: set to 1 after init
    protected variable initialized_ 0

    # resizing during configure, so don't update
    protected variable resizing_ 0
}

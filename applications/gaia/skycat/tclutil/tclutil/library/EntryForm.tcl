# E.S.O. - VLT project/ESO Archive
# "@(#) $Id: EntryForm.tcl,v 1.9 1998/10/28 17:46:37 abrighto Exp $"
#
# EntryForm.tcl - Form dialog for entering data at given labels
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 26 Jun 96   created


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
	pack $itk_component(title) -side top -fill x -expand 1 -pady 2m -padx 2m

	# Frame containing entries.
	itk_component add entries {
	    set f [frame $w_.entries -bd 3 -relief groove]
	}
	pack $f -side top -fill both -expand 1 \
	    -padx 1m -pady 1m -ipadx 1m -ipady 1m
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
	blt::table configure $f c0 -resize none

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
	
	wm title $w_ {Entry Form}
	set initialized_ 1
    }

   
    # reset to the original values
    
    public method reset {} {
	set row 0
	foreach i $itk_option(-values) {
	    set w $w_.entries.entry$row
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


    # -- protected vars --
    
    # array(label) of entry widget for given label
    protected variable entries_

    # flag: set to 1 after init
    protected variable initialized_ 0
}



    

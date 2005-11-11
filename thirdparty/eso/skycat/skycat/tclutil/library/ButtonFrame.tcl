# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: ButtonFrame.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $"
#
# ButtonFrame.tcl - Widget displaying a frame with some standard buttons.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual ButtonFrame {}

# This class is an itcl widget for displaying a frame with the standard
# buttons like "OK Cancel", with options for configuring other labels and 
# actions

itcl::class util::ButtonFrame {
    inherit util::FrameWidget


    # create a ButtonFrame

    constructor {args} {
	itk_option add hull.borderwidth hull.relief
	eval itk_initialize $args
    }


    # add a button to the row or configure an existing button

    public method buttonconfig {label args} {
	if {[info exists but_($label)]} {
	    eval "[list $but_($label) config -text $label] $args"
	} else {
	    pack \
		[set but_($label) [eval "[list button $w_.but$seq_ -text $label] $args"]] \
	    -side left -fill none -expand 1 -padx 3m -pady 2m
	    incr seq_
	}
    }

    # add a button

    public method append {label cmd} {
	buttonconfig $label -text $label -command $cmd
    }

    
    # -- options --

    # labels and commands for the standard buttons
    itk_option define -ok_label ok_label Ok_label {OK} {
	buttonconfig $itk_option(-ok_label) -text $itk_option(-ok_label)
    }

    itk_option define -ok_cmd ok_cmd Ok_cmd {} {
	buttonconfig $itk_option(-ok_label) -command $itk_option(-ok_cmd)
    }

    itk_option define -cancel_label cancel_label Cancel_label {Cancel} {
 	buttonconfig $itk_option(-cancel_label) -text $itk_option(-cancel_label)
    }

    itk_option define -cancel_cmd cancel_cmd Cancel_cmd {} {
	if {"$itk_option(-cancel_cmd)" == ""} {
	    config -cancel_cmd "destroy [winfo toplevel $w_]"
	} else {
	    buttonconfig $itk_option(-cancel_label) -command $itk_option(-cancel_cmd)
	}
    }

    # -- protected variable vars --

    # counter for button names
    protected variable seq_ {0}

    # array mapping label name to button name
    protected variable but_
}


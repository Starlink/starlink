# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: InputDialog.tcl,v 1.7 1998/10/28 17:46:38 abrighto Exp $"
#
# InputDialog.tcl - Dialog to display a message and get input from the user
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created


itk::usual InputDialog {}

# An InputDialog is a dialog to display a message and get input from
# the user.

itcl::class util::InputDialog {
    inherit util::DialogWidget

    # create the dialog

    constructor {args} {
	# add an entry widget below the message
	global ::$variable_
	
	# Tk entry widget for input.
	itk_component add entry {
	    entry $itk_component(ext).entry -relief sunken
	}
	pack $itk_component(entry) -side left -fill x -expand 1 \
		-padx 2m -pady 2m -ipady 1m

	eval itk_initialize $args
    }

    
    # called after options have been evaluated

    protected method init {} {
	DialogWidget::init
	bind $itk_component(entry) <Return> \
		"$itk_component(button$itk_option(-default)) flash; set $variable_ $itk_option(-default)"
    }


    # this method is redefined here to change the value
    # that is returned from activate to be the contents of the entry widget

    protected method set_result {} {
	global ::$variable_
	if {"[set $variable_]" == "$itk_option(-default)"} {
	    return [$itk_component(entry) get]
	}
	return {}
    }
}


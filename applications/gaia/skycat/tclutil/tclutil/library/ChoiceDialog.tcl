# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: ChoiceDialog.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $"
#
# ChoiceDialog.tcl - Dialog to display a message and get choice from the user
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created


itk::usual ChoiceDialog {}

# A ChoiceDialog is a dialog widget used to display a message and 
# get a choice from the user.

itcl::class util::ChoiceDialog {
    inherit util::DialogWidget

    # create the dialog

    constructor {args} {
	# LabelChoice(n) Itk widget
	itk_component add choice {
	    util::LabelChoice $itk_component(ext).choice \
		-command [code $this set_choice]
	} {
	    keep -choice -value -rows -cols 
	}
	pack $itk_component(choice) -side left -fill x -expand 1 \
		-padx 2m -pady 2m -ipady 1m

	eval itk_initialize $args
    }

    
    # called when a choice is made
    
    public method set_choice {choice} {
	global ::$variable_
	set $variable_ $choice
    }

    
    # this method is redefined here to change the value
    # that is returned from activate to be the contents of the choice widget

    protected method set_result {} {
	global ::$variable_
	return [$itk_component(choice) get]
    }
}


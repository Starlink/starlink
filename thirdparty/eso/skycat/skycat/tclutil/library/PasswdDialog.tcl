# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: PasswdDialog.tcl,v 1.1.1.1 2006/01/12 16:40:55 abrighto Exp $"
#
# PasswdDialog.tcl - Dialog to display a message and get a username 
#                    and password from the user.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  15 Jul 98  Created


itk::usual InputDialog {}

# A PasswdDialog is a dialog to display a message and get a username and
# password from the user.

itcl::class util::PasswdDialog {
    inherit util::DialogWidget

    # create the dialog

    constructor {args} {
	# add 2 LabelEntry widgets below the message
	
	# LabelEntry widget for the username.
	itk_component add username {
	    LabelEntry $itk_component(ext).username \
		-text "User ID:" \
		-labelwidth 10
	}
	# LabelEntry widget for the password
	itk_component add passwd {
	    LabelEntry $itk_component(ext).passwd \
		-text "Password:" \
		-labelwidth 10 \
		-show *
	}
	pack \
	    $itk_component(username) \
	    $itk_component(passwd) \
	    -side top -fill x -expand 1 -padx 2m -pady 2m -ipady 1m

	eval itk_initialize $args
    }

    
    # called after options have been evaluated

    protected method init {} {
	DialogWidget::init
	bind [$itk_component(username) component entry] <Return> \
	    "focus [$itk_component(passwd) component entry]; break"
	bind [$itk_component(passwd) component entry] <Return> \
		"$itk_component(button$itk_option(-default)) flash; set $variable_ $itk_option(-default)"
    }


    # this method is redefined here to change the value
    # that is returned from activate to be the contents of the entry widget

    protected method set_result {} {
	global ::$variable_
	if {"[set $variable_]" == "$itk_option(-default)"} {
	    return [list [$itk_component(username) get] [$itk_component(passwd) get]]
	}
	return {}
    }
}


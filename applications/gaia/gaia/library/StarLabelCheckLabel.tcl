#+
#  Name:
#    StarLabelCheckLabel
#
#  Type of Module:
#    [incr Tcl] class
#
#  Purpose:
#    A labelled check box with accompanying editable label.  If the
#    checkbox is `off', the editable box is disabled.
#
#  Description:
#    A class of mega-widget that provides a label, a checkbox, and an
#    editable text field.
#
#    This is similar to the CheckEntry widget, except that it is
#    derived from LabelWidget, and it has the -flagvariable option to
#    manage a flag variable as well as the entry variable.
#
#  Invocations:
#
#    StarLabelCheckLabel object_name [configuration options]
#
#  Configuration options:
#
#    The variable associated with the checkbox is specified with
#    -flagvariable, and that associated with the editable field is
#    -valuevariable.  Otherwise, it has the standard LabelWidget
#    options.
#
#  Inheritance:
#    LabelWidget.  See this for other possible methods and
#    configuration options.
#
#  Authors:
#    NG: Norman Gray (Starlink, Glasgow)
#
#  History:
#    18-Nov-1999 (NG)
#      Original version
#
#  RCS id:
#    $Id$
#-

itk::usual StarLabelCheckLabel {}

itcl::class gaia::StarLabelCheckLabel {

    #  Inheritance
    inherit util::LabelWidget

    #  Constructor
    constructor {args} {
	itk_component add button {
	    checkbutton $w_.button \
		    -disabledforeground "" \
		    -command [code $this toggle_enabled_]
	} {
	    #usual
	    keep -state
	    rename -variable -flagvariable flagVariable FlagVariable
	}
	itk_component add editit {
	    entry $w_.editit
	} {
	    usual
	    keep -state
	    rename -textvariable -valuevariable valueVariable ValueVariable
	}
	pack $itk_component(label) -side left -ipadx 1m
	pack $itk_component(button) -side left -padx 1m -ipadx 1m
	pack $itk_component(editit) -side left -padx 1m -ipadx 1m

	# handle unprocessed configuration options
	eval itk_initialize $args

	toggle_enabled_
    }

    #  Destructor
    destructor {
    }

    #  Methods

    #  Invoke checkbutton.
    method invoke {} {
	$itk_component(button) invoke
    }

    #  Toggle the selection state of the button.
    method toggle {} {
	$itk_component(button) toggle
    }

    #  Select the button.
    method select {} {
	$itk_component(button) select
    }

    #  Deselect the button.
    method deselect {} {
	$itk_component(button) deselect
    }

    private method toggle_enabled_ {} {
	eval set flagvar $$itk_option(-flagvariable)
	#puts "toggle_enabled_: $itk_option(-flagvariable) = $flagvar"
	if {$flagvar} {
	    #puts "   enabled"
	    $itk_component(editit) configure \
		    -state normal \
		    -foreground black
	} else {
	    #puts "   disabled"
	    $itk_component(editit) configure \
		    -state disabled \
		    -foreground gray
	}
    }
}

# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelWidget.tcl,v 1.6 1998/10/28 17:46:39 abrighto Exp $"
#
# LabelWidget.tcl - Base class of labeled widgets
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

# LabelWidget is an itcl widget for displaying a label. 
# Another widget may be added in a derived class...

itcl::class util::LabelWidget {
    inherit util::FrameWidget

    # constructor

    constructor {args} {

	# label for the widget
	itk_component add label {
	    label $w_.label
	} {
	    keep -text -background -foreground -anchor
	    rename -width -labelwidth labelWidth LabelWidth
	    rename -font -labelfont labelFont LabelFont
	}

	eval itk_initialize $args
    }


    # -- options --

    # disabled foreground color
    itk_option define -disabledforeground disabledforeground DisabledForeground {grey}

    # set the state to normal or disabled (greyed out)
    itk_option define -state state State normal {
	if {"$itk_option(-state)" == "normal"} {
	    $itk_component(label) config -foreground $itk_option(-foreground)
	} else {
	    $itk_component(label) config -foreground $itk_option(-disabledforeground)
	}
    }
    
    # widget orientation: horizontal, vertical
    itk_option define -orient orient Orient {horizontal} {
	if {"$itk_option(-orient)" == "horizontal"} {
	    set side_ left
	} else {
	    set side_ top
	}
	if {"$itk_option(-text)" != ""} {
	    pack $itk_component(label) -side $side_ -fill x -ipadx 1m
	}
    }
    
    # pack option
    protected variable side_ {left}
}




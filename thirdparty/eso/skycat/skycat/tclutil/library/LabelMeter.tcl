# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelMeter.tcl,v 1.1 2005/02/01 04:19:07 brighton Exp $"
#
# LabelMeter.tcl - Itk widget for displaying a labeled tixMeter widget
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual LabelMeter {}

# This widget displays a label and a tixMeter widget

itcl::class util::LabelMeter {
    inherit util::LabelWidget

    # constructor: create a new LabelMeter widget

    constructor {args} {
	itk_component add meter {
	    tixMeter $w_.meter
	} {
	    keep -value
	}

	eval itk_initialize $args
    }


    # -- options --

    # set the width of the value displayed
    itk_option define -valuewidth valueWidth ValueWidth {15} {
	$itk_component(meter) config -width $itk_option(-valuewidth)
    }

    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
	if {"$itk_option(-valuewidth)" == "" || $itk_option(-valuewidth) == 0} {
	    set expand 0
	} else {
	    set expand 1
	}
	pack $itk_component(meter) \
	    -side $side_ -expand $expand -fill x -padx 1m -ipadx 1m
    }

}

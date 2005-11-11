# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: CanvasWidget.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $"
#
# CanvasWidget.tcl - Itcl class based on the Tk canvas widget
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual CanvasWidget {}

# This is an itcl class based on the Tk canvas widget. It combines
# a canvas window in a frame with optional scrollbars.

itcl::class util::CanvasWidget {
    inherit util::FrameWidget

    # create the canvas and scrollbars

    constructor {args} {
	# Canvas widget
	itk_component add canvas {
	    canvas $w_.canvas -scrollregion {0 0 0 0}
	} {
	    keep -width -height -borderwidth -relief
	    usual
	    rename -background -canvasbackground canvasBackground CanvasBackground
	}

	# Vertical scrollbar.
	itk_component add vscroll {
	    scrollbar $w_.vscroll \
		-relief sunken \
		-command "$w_.canvas yview"
	}
	
	# Horrizontal scrollbar
	itk_component add hscroll {
	    scrollbar $w_.hscroll \
		-orient horiz \
		-relief sunken \
		-command "$itk_component(canvas) xview"
	}
	$itk_component(canvas) config \
		-xscrollcommand "$itk_component(hscroll) set" \
		-yscrollcommand "$itk_component(vscroll) set"
	
	bind $itk_component(canvas) <ButtonPress-2> "$itk_component(canvas) scan mark %x %y"
	bind $itk_component(canvas) <B2-Motion> "$itk_component(canvas) scan dragto %x %y"

	pack $itk_component(hscroll) -side bottom -fill x
	pack $itk_component(vscroll) -side right -fill y
	pack $itk_component(canvas) -fill both -expand 1

	eval itk_initialize $args
    }
}

# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: TextDialog.tcl,v 1.6 1998/10/28 17:46:41 abrighto Exp $"
#
# TextDialog.tcl - Dialog widget displaying a text window
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created


itk::usual TextDialog {}

# TextDialog is a dialog widget for displaying a text window and a specified
# text.

itcl::class util::TextDialog {
    inherit util::DialogWidget

    # create the dialog

    constructor {args} {
	# Frame for text widget.
	itk_component add textf {
	    set f [frame $itk_component(ext).textf]
	} {
	}
	pack $f -side top -fill both -expand 1
	
	# Tk text widget
	itk_component add textwin {
	    text $f.textwin -yscrollcommand "$f.scroll set" -setgrid 1 -width 80
	} {
	    rename -width -textwidth textWidth TextWidth
	    rename -height -textheight textHeight TextHeight
	    rename -state -textstate textState TextState
	    rename -font -textfont textFont TextFont
	}
	pack $itk_component(textwin) \
	    -side left -fill x -expand 1 

	# vertical scrollbar
	itk_component add vscroll {
	    scrollbar $f.scroll \
		  -command "$f.textwin yview"
	}
	pack $itk_component(vscroll) \
	    -side right -fill y

	eval itk_initialize $args
    }

    
    # -- options --

    # contents of the text window
    itk_option define -contents contents Contents {} {
	$itk_component(textwin) config -state normal
	$itk_component(textwin) delete 1.0 end
	$itk_component(textwin) insert 1.0 $itk_option(-contents)
	$itk_component(textwin) config -state $itk_option(-textstate)
    }
}


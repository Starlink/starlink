itk::usual LabelButton {}

# This widget displays a label and a button. A command may be associated
# with the button.

itcl::class gaia::LabelButton {
    inherit util::LabelWidget

    #  create a LabelButton
    constructor {args} {
	# The button
	itk_component add b {
	    button $w_.b 
	} {
	    keep -relief -borderwidth -state -command -textvariable
	    rename -text -buttontext buttonText Text
	    rename -bitmap -buttonbitmap buttonbitmap Bitmap
	    rename -width -valuewidth valueWidth Width
	    rename -font -valuefont valueFont Font
	    rename -anchor -valueanchor valueAnchor ValueAnchor
	    ignore -disabledforeground
	}

	set default_bg_ [$itk_component(b) cget -background]
	eval itk_initialize $args
    }

    
    # options

    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
	pack $itk_component(b) \
	    -side $side_ -fill x -expand 1 -padx 1m -ipadx 1m
    }

    # -- protected variables --

    # default background color
    protected variable default_bg_

}



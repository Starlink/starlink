# E.S.O. - VLT project/ ESO Archive
# # "@(#) $Id: LabelMessage.tcl,v 1.1.1.1 2006/01/12 16:40:51 abrighto Exp $"
#
# LabelMessage.tcl - Itcl widget for displaying a label and a message
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created


itk::usual LabelMessage {}

# LabelMessage is an Itcl widget for displaying a label and a message.

itcl::class util::LabelMessage {
    inherit util::LabelWidget


    #  Get the value in the message

    public method get {} {
	return [$itk_component(message) cget -text]
    }
   

    # constructor: create a new LabelMessage widget

    constructor {args} {
	# Tk message component
	itk_component add message {
	    message $w_.message
	} {
	    keep -background -foreground -relief -borderwidth \
		-textvariable -anchor -aspect -justify
	    rename -width -valuewidth valueWidth Width
	    rename -font -valuefont valueFont Font
	    rename -text -value value Value
	}

	eval itk_initialize $args
    }

   
    # -- options --


    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
	pack $itk_component(message) \
	    -side $side_ -fill x -padx 1m -ipadx 1m

    }
}


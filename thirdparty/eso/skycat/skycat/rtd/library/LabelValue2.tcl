# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelValue2.tcl,v 1.1.1.1 2006/01/12 16:38:19 abrighto Exp $"
#
# LabelValue2.tcl - Widget displaying a label and two values
#
# who             when       what
# --------------  ---------  ----------------------------------------
# pbiereic        01/03/01   Created


# This widget displays a label and two values.
# Class 'LabelValue' already creates a label and an entry.
# This class adds an additional entry widget packed after the LabelValue object


itk::usual LabelValue2 {}

itcl::class util::LabelValue2 {
    inherit util::LabelValue

    constructor {args} {
	eval itk_initialize $args
    }
    
    protected method init {} {
	$w_ config -disabledforeground black -state disabled
	pack $w_ -fill x -expand 0

	# add an additional entry
	itk_component add entry2 {
	    entry $w_.entry2 -justify right -textvariable {}
	} {
	    keep -textvariable -relief -borderwidth -show
	    rename -font  -valuefont valueFont ValueFont
            rename -width -valuewidth valueWidth ValueWidth
	}
	$itk_component(entry) config -textvariable [cget -textvariable] -justify right
	config -value2 [cget -value2]
	pack $w_ $itk_component(entry2) \
		-anchor w -expand 1 -fill x -padx 1m -ipadx 1m
	$itk_component(entry2) config -textvariable $itk_option(-textvariable2)
   }
    
    # -- options --
    
    itk_option define -shelp shelp Shelp {} {
	add_short_help $w_ [cget -shelp]
    }

    itk_option define -textvariable2 textvariable2 Textvariable2 {}
    
    # set the value displayed in the second entry
    itk_option define -value2 value2 Value2 {} {
	if {[info exists itk_component(entry2)]} {
	    $itk_component(entry2) config -state normal
	    $itk_component(entry2) delete 0 end
	    $itk_component(entry2) insert 0 $itk_option(-value2)
	    $itk_component(entry2) config -state disabled
	}
    }
}

# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelNumber.tcl,v 1.2 2005/02/01 04:19:07 brighton Exp $"
#
# LabelNumber.tcl - Widget displaying a label, a number, and arrow buttons
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual LabelNumber {
    keep -state 
}

# LabelNumber is an Itcl widget for displaying a label, a number
# and buttons to increment and decrement the number.

itcl::class util::LabelNumber {
    inherit util::LabelValue


    #  constructor: create a new LabelNumber

    constructor {args} {
	# button frame
	itk_component add bframe {
	    frame $w_.f
	} {
	    keep -background
	}
	
	# button to increment number.
	itk_component add incr {
	    button $w_.f.incr \
		-bitmap incr  \
		-command [code $this increment 1]
	} {
	    keep -state -background -foreground
	}

	# button to decrement number.
	itk_component add decr {
	    button $w_.f.decr \
		-bitmap decr  \
		-command [code $this increment -1]
	} {
	    keep -state -background -foreground
	}

	pack $itk_component(incr) $itk_component(decr) \
	    -side top

	eval itk_initialize $args

	if {"itk_option(-value)" == ""} {
	    config -value $itk_option(-min)
	}
    }


    # increment (1) or decrement (-1) the value by the current increment

    protected method increment {sign} {
	set v [expr [get]+($sign*$itk_option(-increment))]
	if {$v >= $itk_option(-min) && $v <= $itk_option(-max)} {
	    config -value [format "%g" $v]
	    if {"$itk_option(-command)" != ""} {
		set cmd $itk_option(-command)
		lappend cmd $v
		eval $cmd
	    }
	}
    }


    # -- options -- 

    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
	pack $itk_component(bframe) -side $side_
    }


    # maximum value
    itk_option define -max max Max {100}

    # minimum value
    itk_option define -min min Min {0}

    # amount to add or subtract for each button push
    itk_option define -increment increment Increment {1}
    
    # commands to evaluate whenever the entry value changes
    itk_option define -command command Command {}
}


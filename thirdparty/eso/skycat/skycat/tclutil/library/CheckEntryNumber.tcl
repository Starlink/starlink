# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: CheckEntryNumber.tcl,v 1.1.1.1 2006/01/12 16:40:46 abrighto Exp $"
#
# CheckEntry.tcl - Itcl widget combining a checkbutton and an number entry 
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual CheckEntryNumber {}

# This class defines an Itk widget combining a checkbutton and a number entry.

itcl::class util::CheckEntryNumber {
    inherit util::CheckEntry


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

	pack $itk_component(bframe) -side left

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

    # maximum value
    itk_option define -max max Max {100}

    # minimum value
    itk_option define -min min Min {0}

    # amount to add or subtract for each button push
    itk_option define -increment increment Increment {1}
}




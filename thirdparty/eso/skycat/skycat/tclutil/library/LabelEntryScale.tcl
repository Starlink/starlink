# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelEntryScale.tcl,v 1.1.1.1 2006/01/12 16:40:52 abrighto Exp $"
#
# LabelEntryScale.tcl - Widget combining a labeled entry with a scale widget
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created
#
#                 23 Mar 98  Added changes from P.W. Draper, Starlink.
#                            Sorted out alignment when packing
#                            vertically, added -resolution to keep
#                            list for scale, added bindings for auto
#                            increment and decrement when pressing
#                            left and right arrows, added state
#                            configuration option, added check that
#                            value has changed in scaleCmd (The scale
#                            widget has a <Motion> binding that means
#                            this is called repeatably which means
#                            that any -commands are also called for no
#                            reason).


itk::usual LabelEntryScale {}

# LabelEntryScale is an Itcl widget combining a labeled entry with a 
# scale widget to make a scale with an editable entry field.

itcl::class util::LabelEntryScale {
    inherit util::LabelEntry

    # constructor: create a new instance of this widget

    constructor {args} {
	# Tk frame containing scale widget.
	itk_component add scaleframe {
	    frame $w_.scaleframe
	}
	# Tk scale widget.
	itk_component add scale {
	    scale $itk_component(scaleframe).scale \
		-showvalue 0 \
		-orient horizontal \
		-command [code $this scaleCmd]
	} {
	    keep -background -foreground -length -resolution -digits
	    rename -width -scaleWidth scaleWidth ScaleWidth
	}

	# Left arrow button.
	itk_component add left {
	    button $itk_component(scaleframe).left -bitmap left
	} {
	    keep -background -foreground
	}
	bind $itk_component(left) <ButtonPress-1> [code $this start_increment -1]
	bind $itk_component(left) <ButtonRelease-1> [code $this stop_increment]

	# Right arrow button.
	itk_component add right {
	    button $itk_component(scaleframe).right -bitmap right
	} {
	    keep -background -foreground
	}
	bind $itk_component(right) <ButtonPress-1> [code $this start_increment 1]
	bind $itk_component(right) <ButtonRelease-1> [code $this stop_increment]

	eval itk_initialize $args
    }


    #  Start incrementing if not already doing so.
    protected method start_increment {sign} {
       if { $afterId_ == {} && $itk_option(-state) == "normal" } {
          increment $sign
       }
    }

    #  Stop incrementing.
    protected method stop_increment {} {
       if { $afterId_ != {} } {
          after cancel $afterId_
          set afterId_ {}
       }
    }
    
    # increment (1) or decrement (-1) the value by the current increment

    protected method increment {sign} {
	set v [expr {[get]+($sign*$itk_option(-increment))}]
	if {$v >= $itk_option(-from) && $v <= $itk_option(-to)} {
	    config -value $v
	    if {"$itk_option(-command)" != ""} {
		set cmd $itk_option(-command)
		lappend cmd $v
		eval $cmd
	    }
	}
	set afterId_ [after 200 [code $this increment $sign]]
    }


    # This method is called for changes in the scale widget.
    # It does nothing unless the value has changed, since the Scale has a
    # motion event that is constantly being invoked.

    protected method scaleCmd {newValue} {
       if { $itk_option(-value) != $newValue } { 
	   config -value $newValue
	   if {"$itk_option(-command)" != ""} {
	       eval "$itk_option(-command) $newValue"
	   }
       }
    }
    

    # redefined from parent class to update scale when entry is
    # changed

    private method command_proc_ {cmd} {
	set v [$itk_component(entry) get]
	set from $from_
	set to $to_
	if {$v < $from} {
	    set from $v
	}
	if {$v > $to} {
	    set to $v
	}
	$itk_component(scale) config -from $from -to $to
	scaleCmd $v
    }

    
    # -- options --

    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
       pack $itk_component(scaleframe) -fill x -side $side_
       if {$itk_option(-show_arrows)} {
          pack $itk_component(left) -side left
       }
       pack $itk_component(scale) \
          -side left -expand 1 -fill x -padx 1m -ipadx 1m
       if {$itk_option(-show_arrows)} {
          pack $itk_component(right) -side left
       }
    }
    
    # scale range -from
    itk_option define -from from From {} {
	set v $itk_option(-from)
	if {"$v" != ""} {
	    $itk_component(scale) config -from $v
	    set from_ $v
	}
    }
    
    # scale range -to
    itk_option define -to to To {} {
	set v $itk_option(-to)
	if {"$v" != ""} {
	    $itk_component(scale) config -to $itk_option(-to)
	    set to_ $v
	}
    }
    
    # set the value in the entry
    itk_option define -value value Value {} {
	set v $itk_option(-value)
	if {"$v" != ""} {
	    $itk_component(scale) set $itk_option(-value)
	}
    }

    # flag: if true, display left and right arrows for incrementing the value
    itk_option define -show_arrows show_arrows Show_arrows {0} 

    # amount to add or subtract for each button push
    itk_option define -increment increment Increment 1
    
    # command to execute when the value changes
    itk_option define -command command Command {} 

    # set the state to normal or disabled (greyed out)
    itk_option define -state state State normal {
	$itk_component(scale) config -state $itk_option(-state)
	if {"$itk_option(-state)" == "normal"} {
	    $itk_component(scale) config -foreground $itk_option(-foreground)
	} else {
	    $itk_component(scale) config -foreground $itk_option(-disabledforeground)
	}
    }

    #  State of increment command.
    protected variable afterId_ {}

    # original -from and -to values
    protected variable from_
    protected variable to_
}

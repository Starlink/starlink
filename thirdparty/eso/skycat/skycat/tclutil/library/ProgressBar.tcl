# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: ProgressBar.tcl,v 1.7 1998/10/30 18:18:23 abrighto Exp $"
#
# ProgressBar.tcl - widget to display a message and a progress bar
#                   in a frame while doing some work that may take 
#                   a while...
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  23 Jan 96  Created
#

itk::usual ProgressBar {}

# This object displays a message and a scale indicating that
# work is in progress

itcl::class util::ProgressBar {
    inherit util::FrameWidget

    # create the dialog

    constructor {args} {
	global tcl_version
	itk_option add hull.borderwidth hull.relief

	# entry widget used to display the progress message
	itk_component add entry {
	    entry $w_.entry -relief flat -state disabled -highlightthickness 0
	} {
	    keep -font
	}
	pack $itk_component(entry) -side left -fill x -expand 1 -padx 1m
	    
	itk_component add f {
	    set f [frame $w_.f]
	} 
	pack $f -side right -padx 1m

	# scale widget used to show progress
	itk_component add scale {
	    scale $f.scale \
		-orient horizontal \
		-sliderlength 0 \
		-from 0 \
		-to 1 \
		-showvalue 0 \
		-highlightthickness 0 \
	} {
	    keep -width -length
	}
	
	# something changed in tk8...
	if {$tcl_version >= 8} {
	    $f.scale config \
		-sliderrelief sunken \
		-borderwidth 4 \
	}

	pack $itk_component(scale)

	eval itk_initialize $args
    }

    
    # reset the bar to the beginning
    
    public method reset {{text ""}} {
	look_busy 0
	config -text {}
    }

    
    # start (or stop) some animation to indicate that work is in progress.
    # This will continue until the next reset or until -to is set with config

    public method look_busy {{bool 1}} {
	$itk_component(scale) set [set itk_option(-value) 0]
	$itk_component(scale) config \
	    -from 0 \
	    -to 20 \
	    -troughcolor $itk_option(-idlecolor)

	if {[set looking_busy_ $bool]} {
	    set itk_option(-value) 0
	    $itk_component(scale) config \
		-sliderlength $itk_option(-sliderlength) \
		-troughcolor $itk_option(-busycolor)
	    do_something
	} else {
	    $itk_component(scale) config \
		-sliderlength 0 \
		-troughcolor $itk_option(-busycolor)
	    after cancel [code $this do_something]
	}
    }


    # do something to look busy

    protected method do_something {} {
	if {$looking_busy_} {
	    if {$itk_option(-value) <= 0} {
		set inc_ 1;
	    } elseif {$itk_option(-value) >= 20} {
		set inc_ -1;
	    } 
	    incr itk_option(-value) $inc_
	    $itk_component(scale) set $itk_option(-value)
	    update
	    after $itk_option(-speed) [code $this do_something]
	}
    }

    
    # -- options --

    # set the text of the label
    itk_option define -text text Text {} {
	$itk_component(entry) config -state normal
	$itk_component(entry) delete 0 end
	$itk_component(entry) insert 0 $itk_option(-text)
	$itk_component(entry) config -state disabled
    }
    
    # set the value of the bar between from and to
    itk_option define -value value Value 0 {
	$itk_component(scale) set $itk_option(-value)
	if {$looking_busy_} {
	    look_busy 0
	}

    }
        
    # set the starting point for the scale
    itk_option define -from from From {0} {
	$itk_component(scale) config -from [expr $itk_option(-from) - 1]
    }

    # set the end point for the scale
    itk_option define -to to To {20} {
	look_busy 0
	$itk_component(scale) config \
	    -to [expr $itk_option(-to) + 1] \
	    -troughcolor $itk_option(-busycolor)
    }

    # color of bar when busy
    itk_option define -busycolor busyColor BusyColor {blue} 

    # color of bar when busy
    itk_option define -idlecolor idleColor IdleColor {grey80}

    # speed in ms/tick when looking busy
    itk_option define -speed speed Speed 250

    # length of slider (displayed when busy)
    itk_option define -sliderlength sliderLength SliderLength 25

    
    # -- protected vars --
    
    # current bar color
    protected variable color_ {grey80}

    # flag: true if currently looking busy
    protected variable looking_busy_ 0
    
    # controls direction for busy animation
    protected variable inc_ 1
}


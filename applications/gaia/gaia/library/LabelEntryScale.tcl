# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id$"

#
# LabelEntryScale.tcl - Widget combining a labeled entry with a scale widget
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created
# P.W. Draper     1996       Sorted out alignment when packing
#                            vertically. Added -resolution to keep
#                            list for scale. Added bindings for auto
#                            increment and decrement when pressing
#                            left and right arrows. Added state
#                            configuration option. 
#                 1997       Added check that value has changed in
#                            scaleCmd. The scale widget has a <Motion>
#                            binding that means this is called
#                            repeatably which means that any -commands
#                            are also called for no reason.
#                 1998       Added option to hide the scale and just 
#                            keep the arrows. Also added option to 
#                            constrain the upper and lower value
#                            limits.


itk::usual LabelEntryScale {}

# LabelEntryScale is an Itcl widget combining a labeled entry with a 
# scale widget to make a scale with an editable entry field.

class util::LabelEntryScale {
    inherit util::LabelEntry

    # constructor: create a new instance of this widget

    constructor {args} {
       itk_component add scaleframe {
          frame $w_.scaleframe
       }
       itk_component add scale {
          scale $itk_component(scaleframe).scale \
             -showvalue 0 \
             -orient horizontal \
             -command [code $this scaleCmd]
       } {
          keep -background -foreground -from -to -length -resolution
          rename -width -scaleWidth scaleWidth ScaleWidth
       }

       itk_component add left {
          button $itk_component(scaleframe).left
       } {
          keep -background -foreground
       }
       bind $itk_component(left) <ButtonPress-1> [code $this start_increment -1]
       bind $itk_component(left) <ButtonRelease-1> [code $this stop_increment]

       itk_component add right {
          button $itk_component(scaleframe).right
       } {
          keep -background -foreground
       }
       bind $itk_component(right) <ButtonPress-1> [code $this start_increment 1]
       bind $itk_component(right) <ButtonRelease-1> [code $this stop_increment]

       eval itk_initialize $args
    }


    #  Start incrementing if not already doing so.
    method start_increment {sign} {
       if { $afterId_ == {} && $itk_option(-state) == "normal" } {
          increment $sign
       }
    }

    #  Stop incrementing.
    method stop_increment {} {
       if { $afterId_ != {} } {
          after cancel $afterId_
          set afterId_ {}
       }
    }

    # increment (1) or decrement (-1) the value by the current increment

    method increment {sign} {
       set v [expr [get]+($sign*$itk_option(-increment))]
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

    # called for changes in the scale widget

    method scaleCmd {newValue} {
       # $itk_component(scale) set $newValue

       #  Do nothing unless the value has changed. The Scale has a
       #  Motion event that is constantly being invoked.
       if { $itk_option(-value) != $newValue } { 
          config -value $newValue
          if {"$itk_option(-command)" != ""} {
             eval "$itk_option(-command) $newValue"
          }
       }
    }


    # redefined from parent class to update scale when entry is
    # changed

    method command_proc_ {cmd} {
       set v [$itk_component(entry) get]
       if { $itk_option(-fix_range) } {
          if {$v < $from_} {
             set v $from_
          }
          if {$v > $to_} {
             set v $to_
          }
          scaleCmd $v
          configure -value $v
       } else {
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
    }

    # -- options --

    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
       if { $itk_option(-show_scale) } {
          set arrowside left
          set bitmap1 left
          set bitmap2 right
       } else {
          set arrowside top
          set bitmap1 incr
          set bitmap2 decr
       }
       pack $itk_component(scaleframe) -fill x -side $side_
       if {$itk_option(-show_arrows)} {
          pack $itk_component(left) -side $arrowside
          $itk_component(left) configure -bitmap $bitmap1
       }
       if { $itk_option(-show_scale) } {
          pack $itk_component(scale) \
             -side left -expand 1 -fill x -padx 1m -ipadx 1m
       }
       if {$itk_option(-show_arrows)} {
          pack $itk_component(right) -side $arrowside
          $itk_component(right) configure -bitmap $bitmap2
       }
    }


    # scale range -from
    itk_option define -from from From {} {
       set v $itk_option(-from)
       $itk_component(scale) config -from $v
       set from_ $v
    }

    # scale range -to
    itk_option define -to to To {} {
       set v $itk_option(-to)
       $itk_component(scale) config -to $itk_option(-to)
       set to_ $v
    }

    # set the value in the entry
    itk_option define -value value Value {} {
	$itk_component(scale) set $itk_option(-value)
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
	# propagate the change
	# ??? LabelEntry::config -state $itk_option(-state) doesn't work!
	$itk_component(entry) config -state $itk_option(-state)
	if {"$itk_option(-state)" == "normal"} {
	    $itk_component(entry) config -foreground $itk_option(-foreground)
	} else {
	    $itk_component(entry) config -foreground $itk_option(-disabledforeground)
	}
    }

    # whether to show the scale widget.
    itk_option define -show_scale show_scale Show_Scale 1 {}

    # whether to/from range is also fixed in entry field.
    itk_option define -fix_range fix_range Fix_Range 0 {}

    #  State of increment command.
    protected variable afterId_ {}

    # original -from and -to values
    protected variable from_
    protected variable to_
}

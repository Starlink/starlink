# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelEntryScale.tcl,v 1.9 2006/06/27 16:22:47 pwd Exp $"
#
# LabelEntryScale.tcl - Widget combining a labeled entry with a scale widget
#
#  Copyright:
#     Copyright (C) 1999-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.
#
#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA
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
# Peter W. Draper 26 Feb 99  More changes: added option to hide the
#                            scale and just keep the arrows. Also
#                            added option to constrain the upper and
#                            lower value limits.
#                 31 Mar 06  Added bindscale method.


itk::usual LabelEntryScale {}

# LabelEntryScale is an Itcl widget combining a labeled entry with a 
# scale widget to make a scale with an editable entry field.

itcl::class util::LabelEntryScale {
    inherit util::LabelEntry

    # constructor: create a new instance of this widget

    constructor {args} {

        # Remove value from configuration options as we may need to 
        # update the range of the scale.
        itk_option remove util::LabelEntry::value

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
	    button $itk_component(scaleframe).left
	} {
	    keep -background -foreground
	}
	bind $itk_component(left) <ButtonPress-1> [code $this start_increment -1]
	bind $itk_component(left) <ButtonRelease-1> [code $this stop_increment]

	# Right arrow button.
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
    
    #  Increment (1) or decrement (-1) the value by the current increment
    protected method increment {sign} {
	set v [expr [get]+($sign*$itk_option(-increment))]
	if {$v >= $itk_option(-from) && $v <= $itk_option(-to)} {
	    config -value $v
	    if {"$itk_option(-command)" != ""} {
		set cmd $itk_option(-command)
		lappend cmd $v
		eval $cmd
	    }
	}
	set afterId_ [after $itk_option(-delay) [code $this increment $sign]]
    }


    #  This method is called for changes in the scale widget. It does
    #  nothing unless the value has changed, since the Scale has a
    #  motion event that is constantly being invoked.
    protected method scaleCmd {newValue} {
       if { $itk_option(-value) != $newValue } { 
	   config -value $newValue
	   if {"$itk_option(-command)" != ""} {
	       eval "$itk_option(-command) $newValue"
	   }
       }
    }
    

    #  Redefined from parent class to update scale when entry is
    #  changed
    private method command_proc_ {cmd} {
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
   
    #  Add a binding to all widgets related to scale. If a binding exists
    #  already this one will be appended to it.
    public method bindscale {sequence script} {
       bind $itk_component(scale) $sequence +"$script"
       bind $itk_component(left) $sequence +"$script"
       bind $itk_component(right) $sequence +"$script"
    }

    #  Add a binding to all widgets related to entry field. If a binding
    #  exists already this one will be appended to it.
    public method bindentry {sequence script} {
       bind $itk_component(entry) $sequence +"$script"
    }

    
    # -- options --

    #  Widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
       if { $itk_option(-show_scale) } {
          set arrowside left
          set bitmap1 left
          set bitmap2 right
       } else {
          set arrowside bottom
          set bitmap1 decr
          set bitmap2 incr
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
    
    #  Scale range -from.
    itk_option define -from from From {} {
	set v $itk_option(-from)
	if {"$v" != ""} {
	    $itk_component(scale) config -from $v
	    set from_ $v
	}
    }
    
    #  Scale range -to.
    itk_option define -to to To {} {
	set v $itk_option(-to)
	if {"$v" != ""} {
	    $itk_component(scale) config -to $itk_option(-to)
	    set to_ $v
	}
    }
    
    #  Set the value for scale and entry field.
    itk_option define -value value Value {} {
	set v $itk_option(-value)
        if {"$v" != ""} {
           set notrace_ 1
           #  Update range to reflect this, if possible. Assumes -to
           #  -from already set.
           set from $itk_option(-from)
           set to $itk_option(-to)
           if { ! $itk_option(-fix_range) } {
              #  Expand range, if needed.
              if { $v < $from} {
                 set from $v
              }
              if { $v > $to } {
                 set to $v
              }
              if { $from != {} && $to != {} } { 
                 config -from $from -to $to
              }
           } else {
              #  Clip to range.
              if { $v < $from} {
                 set v $from
              }
              if { $v > $to } {
                 set v $to
              }
              set itk_option(-value) $v
           }
           
           #  Now update values.
           set prev_state [$itk_component(entry) cget -state]
           $itk_component(entry) config -state normal
           $itk_component(entry) delete 0 end
           $itk_component(entry) insert 0 $v
           $itk_component(scale) set $v
           if {"$itk_option(-justify)" == "right"} {
              $itk_component(entry) icursor end
              $itk_component(entry) xview moveto 1
           }
           $itk_component(entry) config -state $prev_state
           set notrace_ 0
        }
    }

    #  Flag: if true, display left and right arrows for incrementing the value
    itk_option define -show_arrows show_arrows Show_arrows {0} 

    #  Amount to add or subtract for each button push
    itk_option define -increment increment Increment 1
    
    #  Command to execute when the value changes
    itk_option define -command command Command {} 

    #  Set the state to normal or disabled (greyed out)
    itk_option define -state state State normal {
	$itk_component(scale) config -state $itk_option(-state)
	if {"$itk_option(-state)" == "normal"} {
	    $itk_component(scale) config -foreground $itk_option(-foreground)
	} else {
	    $itk_component(scale) config -foreground $itk_option(-disabledforeground)
	}
    }

    #  Whether to/from range is also fixed in entry field.
    itk_option define -fix_range fix_range Fix_Range 0 {}

    #  Whether to show the scale widget.
    itk_option define -show_scale show_scale Show_Scale 1 {}

    #  The pause in milliseconds for the animation delay.
    itk_option define -delay delay Delay 150

    #  State of increment command.
    protected variable afterId_ {}

    #  Original -from and -to values
    protected variable from_ {}
    protected variable to_ {}
}

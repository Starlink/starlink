# scale.tcl --
#
# This file defines the default bindings for Tk scale widgets and provides
# procedures that help in implementing the bindings.
#
# SCCS: @(#) scale.tcl 1.12 96/04/16 11:42:25
#
# Copyright (c) 1994 The Regents of the University of California.
# Copyright (c) 1994-1995 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

#-------------------------------------------------------------------------
# The code below creates the default class bindings for entries.
#-------------------------------------------------------------------------

# Standard Motif bindings:

bind Scale <Enter> {
    if $tk_strictMotif {
	set tkPriv(activeBg) [%q cget -activebackground]
	%q config -activebackground [%q cget -background]
    }
    tkScaleActivate %W %x %y
}
bind Scale <Motion> {
    tkScaleActivate %W %x %y
}
bind Scale <Leave> {
    if $tk_strictMotif {
	%q config -activebackground $tkPriv(activeBg)
    }
    if {[%q cget -state] == "active"} {
	%q configure -state normal
    }
}
bind Scale <1> {
    tkScaleButtonDown %W %x %y
}
bind Scale <B1-Motion> {
    tkScaleDrag %W %x %y
}
bind Scale <B1-Leave> { }
bind Scale <B1-Enter> { }
bind Scale <ButtonRelease-1> {
    tkCancelRepeat
    tkScaleEndDrag %W
    tkScaleActivate %W %x %y
}
bind Scale <2> {
    tkScaleButton2Down %W %x %y
}
bind Scale <B2-Motion> {
    tkScaleDrag %W %x %y
}
bind Scale <B2-Leave> { }
bind Scale <B2-Enter> { }
bind Scale <ButtonRelease-2> {
    tkCancelRepeat
    tkScaleEndDrag %W
    tkScaleActivate %W %x %y
}
bind Scale <Control-1> {
    tkScaleControlPress %W %x %y
}
bind Scale <Up> {
    tkScaleIncrement %W up little noRepeat
}
bind Scale <Down> {
    tkScaleIncrement %W down little noRepeat
}
bind Scale <Left> {
    tkScaleIncrement %W up little noRepeat
}
bind Scale <Right> {
    tkScaleIncrement %W down little noRepeat
}
bind Scale <Control-Up> {
    tkScaleIncrement %W up big noRepeat
}
bind Scale <Control-Down> {
    tkScaleIncrement %W down big noRepeat
}
bind Scale <Control-Left> {
    tkScaleIncrement %W up big noRepeat
}
bind Scale <Control-Right> {
    tkScaleIncrement %W down big noRepeat
}
bind Scale <Home> {
    %q set [%q cget -from]
}
bind Scale <End> {
    %q set [%q cget -to]
}

# tkScaleActivate --
# This procedure is invoked to check a given x-y position in the
# scale and activate the slider if the x-y position falls within
# the slider.
#
# Arguments:
# w -		The scale widget.
# x, y -	Mouse coordinates.

proc tkScaleActivate {w x y} {
    global tkPriv
    set q [winfo command $w]
    if {[$q cget -state] == "disabled"} {
	return;
    }
    if {[$q identify $x $y] == "slider"} {
	$q configure -state active
    } else {
	$q configure -state normal
    }
}

# tkScaleButtonDown --
# This procedure is invoked when a button is pressed in a scale.  It
# takes different actions depending on where the button was pressed.
#
# Arguments:
# w -		The scale widget.
# x, y -	Mouse coordinates of button press.

proc tkScaleButtonDown {w x y} {
    global tkPriv
    set q [winfo command $w]
    set tkPriv(dragging) 0
    set el [$q identify $x $y]
    if {$el == "trough1"} {
	tkScaleIncrement $w up little initial
    } elseif {$el == "trough2"} {
	tkScaleIncrement $w down little initial
    } elseif {$el == "slider"} {
	set tkPriv(dragging) 1
	set tkPriv(initValue) [$q get]
	set coords [$q coords]
	set tkPriv(deltaX) [expr $x - [lindex $coords 0]]
	set tkPriv(deltaY) [expr $y - [lindex $coords 1]]
	$q configure -sliderrelief sunken
    }
}

# tkScaleDrag --
# This procedure is called when the mouse is dragged with
# mouse button 1 down.  If the drag started inside the slider
# (i.e. the scale is active) then the scale's value is adjusted
# to reflect the mouse's position.
#
# Arguments:
# w -		The scale widget.
# x, y -	Mouse coordinates.

proc tkScaleDrag {w x y} {
    global tkPriv
    set q [winfo command $w]
    if !$tkPriv(dragging) {
	return
    }
    $q set [$q get [expr $x - $tkPriv(deltaX)] \
	    [expr $y - $tkPriv(deltaY)]]
}

# tkScaleEndDrag --
# This procedure is called to end an interactive drag of the
# slider.  It just marks the drag as over.
#
# Arguments:
# w -		The scale widget.

proc tkScaleEndDrag {w} {
    global tkPriv
    set tkPriv(dragging) 0
    [winfo command $w] configure -sliderrelief raised
}

# tkScaleIncrement --
# This procedure is invoked to increment the value of a scale and
# to set up auto-repeating of the action if that is desired.  The
# way the value is incremented depends on the "dir" and "big"
# arguments.
#
# Arguments:
# w -		The scale widget.
# dir -		"up" means move value towards -from, "down" means
#		move towards -to.
# big -		Size of increments: "big" or "little".
# repeat -	Whether and how to auto-repeat the action:  "noRepeat"
#		means don't auto-repeat, "initial" means this is the
#		first action in an auto-repeat sequence, and "again"
#		means this is the second repetition or later.

proc tkScaleIncrement {w dir big repeat} {
    global tkPriv
    if {![winfo exists $w]} return
    set q [winfo command $w]
    if {$big == "big"} {
	set inc [$q cget -bigincrement]
	if {$inc == 0} {
	    set inc [expr abs([$q cget -to] - [$q cget -from])/10.0]
	}
	if {$inc < [$q cget -resolution]} {
	    set inc [$q cget -resolution]
	}
    } else {
	set inc [$q cget -resolution]
    }
    if {([$q cget -from] > [$q cget -to]) ^ ($dir == "up")} {
	set inc [expr -$inc]
    }
    $q set [expr [$q get] + $inc]

    if {$repeat == "again"} {
	set tkPriv(afterId) [after [$q cget -repeatinterval] \
		tkScaleIncrement $w $dir $big again]
    } elseif {$repeat == "initial"} {
	set delay [$q cget -repeatdelay]
	if {$delay > 0} {
	    set tkPriv(afterId) [after $delay \
		    tkScaleIncrement $w $dir $big again]
	}
    }
}

# tkScaleControlPress --
# This procedure handles button presses that are made with the Control
# key down.  Depending on the mouse position, it adjusts the scale
# value to one end of the range or the other.
#
# Arguments:
# w -		The scale widget.
# x, y -	Mouse coordinates where the button was pressed.

proc tkScaleControlPress {w x y} {
    set q [winfo command $w]
    set el [$q identify $x $y]
    if {$el == "trough1"} {
	$q set [$q cget -from]
    } elseif {$el == "trough2"} {
	$q set [$q cget -to]
    }
}

# tkScaleButton2Down
# This procedure is invoked when button 2 is pressed over a scale.
# It sets the value to correspond to the mouse position and starts
# a slider drag.
#
# Arguments:
# w -		The scrollbar widget.
# x, y -	Mouse coordinates within the widget.

proc tkScaleButton2Down {w x y} {
    global tkPriv

    set q [winfo command $w]
    if {[$q cget -state] == "disabled"} {
	return;
    }
    $q configure -state active
    $q set [$q get $x $y]
    set tkPriv(dragging) 1
    set tkPriv(initValue) [$q get]
    set coords "$x $y"
    set tkPriv(deltaX) 0
    set tkPriv(deltaY) 0
}

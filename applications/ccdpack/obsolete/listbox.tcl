# listbox.tcl --
#
# This file defines the default bindings for Tk listbox widgets
# and provides procedures that help in implementing those bindings.
#
# SCCS: @(#) listbox.tcl 1.19 96/09/17 13:43:19
#
# Copyright (c) 1994 The Regents of the University of California.
# Copyright (c) 1994-1995 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

#--------------------------------------------------------------------------
# tkPriv elements used in this file:
#
# afterId -		Token returned by "after" for autoscanning.
# listboxPrev -		The last element to be selected or deselected
#			during a selection operation.
# listboxSelection -	All of the items that were selected before the
#			current selection operation (such as a mouse
#			drag) started;  used to cancel an operation.
#--------------------------------------------------------------------------

#-------------------------------------------------------------------------
# The code below creates the default class bindings for listboxes.
#-------------------------------------------------------------------------

# Note: the check for existence of %W below is because this binding
# is sometimes invoked after a window has been deleted (e.g. because
# there is a double-click binding on the widget that deletes it).  Users
# can put "break"s in their bindings to avoid the error, but this check
# makes that unnecessary.

bind Listbox <1> {
    if [winfo exists %W] {
	tkListboxBeginSelect %W [%q index @%x,%y]
    }
}

# Ignore double clicks so that users can define their own behaviors.
# Among other things, this prevents errors if the user deletes the
# listbox on a double click.

bind Listbox <Double-1> {
    # Empty script
}

bind Listbox <B1-Motion> {
    set tkPriv(x) %x
    set tkPriv(y) %y
    tkListboxMotion %W [%q index @%x,%y]
}
bind Listbox <ButtonRelease-1> {
    tkCancelRepeat
    %q activate @%x,%y
}
bind Listbox <Shift-1> {
    tkListboxBeginExtend %W [%q index @%x,%y]
}
bind Listbox <Control-1> {
    tkListboxBeginToggle %W [%q index @%x,%y]
}
bind Listbox <B1-Leave> {
    set tkPriv(x) %x
    set tkPriv(y) %y
    tkListboxAutoScan %W
}
bind Listbox <B1-Enter> {
    tkCancelRepeat
}

bind Listbox <Up> {
    tkListboxUpDown %W -1
}
bind Listbox <Shift-Up> {
    tkListboxExtendUpDown %W -1
}
bind Listbox <Down> {
    tkListboxUpDown %W 1
}
bind Listbox <Shift-Down> {
    tkListboxExtendUpDown %W 1
}
bind Listbox <Left> {
    %q xview scroll -1 units
}
bind Listbox <Control-Left> {
    %q xview scroll -1 pages
}
bind Listbox <Right> {
    %q xview scroll 1 units
}
bind Listbox <Control-Right> {
    %q xview scroll 1 pages
}
bind Listbox <Prior> {
    %q yview scroll -1 pages
    %q activate @0,0
}
bind Listbox <Next> {
    %q yview scroll 1 pages
    %q activate @0,0
}
bind Listbox <Control-Prior> {
    %q xview scroll -1 pages
}
bind Listbox <Control-Next> {
    %q xview scroll 1 pages
}
bind Listbox <Home> {
    %q xview moveto 0
}
bind Listbox <End> {
    %q xview moveto 1
}
bind Listbox <Control-Home> {
    %q activate 0
    %q see 0
    %q selection clear 0 end
    %q selection set 0
}
bind Listbox <Shift-Control-Home> {
    tkListboxDataExtend %W 0
}
bind Listbox <Control-End> {
    %q activate end
    %q see end
    %q selection clear 0 end
    %q selection set end
}
bind Listbox <Shift-Control-End> {
    tkListboxDataExtend %W [%q index end]
}
bind Listbox <<Copy>> {
    if {[selection own -displayof %W] == "%W"} {
	clipboard clear -displayof %W
	clipboard append -displayof %W [selection get -displayof %W]
    }
}
bind Listbox <space> {
    tkListboxBeginSelect %W [%q index active]
}
bind Listbox <Select> {
    tkListboxBeginSelect %W [%q index active]
}
bind Listbox <Control-Shift-space> {
    tkListboxBeginExtend %W [%q index active]
}
bind Listbox <Shift-Select> {
    tkListboxBeginExtend %W [%q index active]
}
bind Listbox <Escape> {
    tkListboxCancel %W
}
bind Listbox <Control-slash> {
    tkListboxSelectAll %W
}
bind Listbox <Control-backslash> {
    if {[%q cget -selectmode] != "browse"} {
	%q selection clear 0 end
    }
}

# Additional Tk bindings that aren't part of the Motif look and feel:

bind Listbox <2> {
    %q scan mark %x %y
}
bind Listbox <B2-Motion> {
    %q scan dragto %x %y
}

# tkListboxBeginSelect --
#
# This procedure is typically invoked on button-1 presses.  It begins
# the process of making a selection in the listbox.  Its exact behavior
# depends on the selection mode currently in effect for the listbox;
# see the Motif documentation for details.
#
# Arguments:
# w -		The listbox widget.
# el -		The element for the selection operation (typically the
#		one under the pointer).  Must be in numerical form.

proc tkListboxBeginSelect {w el} {
    global tkPriv
    set q [winfo command $w]
    if {[$q cget -selectmode]  == "multiple"} {
	if [$q selection includes $el] {
	    $q selection clear $el
	} else {
	    $q selection set $el
	}
    } else {
	$q selection clear 0 end
	$q selection set $el
	$q selection anchor $el
	set tkPriv(listboxSelection) {}
	set tkPriv(listboxPrev) $el
    }
}

# tkListboxMotion --
#
# This procedure is called to process mouse motion events while
# button 1 is down.  It may move or extend the selection, depending
# on the listbox's selection mode.
#
# Arguments:
# w -		The listbox widget.
# el -		The element under the pointer (must be a number).

proc tkListboxMotion {w el} {
    global tkPriv
    if {$el == $tkPriv(listboxPrev)} {
	return
    }
    set q [winfo command $w]
    set anchor [$q index anchor]
    switch [$q cget -selectmode] {
	browse {
	    $q selection clear 0 end
	    $q selection set $el
	    set tkPriv(listboxPrev) $el
	}
	extended {
	    set i $tkPriv(listboxPrev)
	    if [$q selection includes anchor] {
		$q selection clear $i $el
		$q selection set anchor $el
	    } else {
		$q selection clear $i $el
		$q selection clear anchor $el
	    }
	    while {($i < $el) && ($i < $anchor)} {
		if {[lsearch $tkPriv(listboxSelection) $i] >= 0} {
		    $q selection set $i
		}
		incr i
	    }
	    while {($i > $el) && ($i > $anchor)} {
		if {[lsearch $tkPriv(listboxSelection) $i] >= 0} {
		    $q selection set $i
		}
		incr i -1
	    }
	    set tkPriv(listboxPrev) $el
	}
    }
}

# tkListboxBeginExtend --
#
# This procedure is typically invoked on shift-button-1 presses.  It
# begins the process of extending a selection in the listbox.  Its
# exact behavior depends on the selection mode currently in effect
# for the listbox;  see the Motif documentation for details.
#
# Arguments:
# w -		The listbox widget.
# el -		The element for the selection operation (typically the
#		one under the pointer).  Must be in numerical form.

proc tkListboxBeginExtend {w el} {
    set q [winfo command $w]
    if {([$q cget -selectmode] == "extended")
	    && [$q selection includes anchor]} {
	tkListboxMotion $w $el
    }
}

# tkListboxBeginToggle --
#
# This procedure is typically invoked on control-button-1 presses.  It
# begins the process of toggling a selection in the listbox.  Its
# exact behavior depends on the selection mode currently in effect
# for the listbox;  see the Motif documentation for details.
#
# Arguments:
# w -		The listbox widget.
# el -		The element for the selection operation (typically the
#		one under the pointer).  Must be in numerical form.

proc tkListboxBeginToggle {w el} {
    global tkPriv
    set q [winfo command $w]
    if {[$q cget -selectmode] == "extended"} {
	set tkPriv(listboxSelection) [$q curselection]
	set tkPriv(listboxPrev) $el
	$q selection anchor $el
	if [$q selection includes $el] {
	    $q selection clear $el
	} else {
	    $q selection set $el
	}
    }
}

# tkListboxAutoScan --
# This procedure is invoked when the mouse leaves an entry window
# with button 1 down.  It scrolls the window up, down, left, or
# right, depending on where the mouse left the window, and reschedules
# itself as an "after" command so that the window continues to scroll until
# the mouse moves back into the window or the mouse button is released.
#
# Arguments:
# w -		The entry window.

proc tkListboxAutoScan {w} {
    global tkPriv
    if {![winfo exists $w]} return
    set x $tkPriv(x)
    set y $tkPriv(y)
    set q [winfo command $w]
    if {$y >= [winfo height $w]} {
	$q yview scroll 1 units
    } elseif {$y < 0} {
	$q yview scroll -1 units
    } elseif {$x >= [winfo width $w]} {
	$q xview scroll 2 units
    } elseif {$x < 0} {
	$q xview scroll -2 units
    } else {
	return
    }
    tkListboxMotion $w [$q index @$x,$y]
    set tkPriv(afterId) [after 50 tkListboxAutoScan $w]
}

# tkListboxUpDown --
#
# Moves the location cursor (active element) up or down by one element,
# and changes the selection if we're in browse or extended selection
# mode.
#
# Arguments:
# w -		The listbox widget.
# amount -	+1 to move down one item, -1 to move back one item.

proc tkListboxUpDown {w amount} {
    global tkPriv
    set q [winfo command $w]
    $q activate [expr [$q index active] + $amount]
    $q see active
    switch [$q cget -selectmode] {
	browse {
	    $q selection clear 0 end
	    $q selection set active
	}
	extended {
	    $q selection clear 0 end
	    $q selection set active
	    $q selection anchor active
	    set tkPriv(listboxPrev) [$q index active]
	    set tkPriv(listboxSelection) {}
	}
    }
}

# tkListboxExtendUpDown --
#
# Does nothing unless we're in extended selection mode;  in this
# case it moves the location cursor (active element) up or down by
# one element, and extends the selection to that point.
#
# Arguments:
# w -		The listbox widget.
# amount -	+1 to move down one item, -1 to move back one item.

proc tkListboxExtendUpDown {w amount} {
    set q [winfo command $w]
    if {[$q cget -selectmode] != "extended"} {
	return
    }
    $q activate [expr [$q index active] + $amount]
    $q see active
    tkListboxMotion $w [$q index active]
}

# tkListboxDataExtend
#
# This procedure is called for key-presses such as Shift-KEndData.
# If the selection mode isn't multiple or extend then it does nothing.
# Otherwise it moves the active element to el and, if we're in
# extended mode, extends the selection to that point.
#
# Arguments:
# w -		The listbox widget.
# el -		An integer element number.

proc tkListboxDataExtend {w el} {
    set q [winfo command $w]
    set mode [$q cget -selectmode]
    if {$mode == "extended"} {
	$q activate $el
	$q see $el
        if [$q selection includes anchor] {
	    tkListboxMotion $w $el
	}
    } elseif {$mode == "multiple"} {
	$q activate $el
	$q see $el
    }
}

# tkListboxCancel
#
# This procedure is invoked to cancel an extended selection in
# progress.  If there is an extended selection in progress, it
# restores all of the items between the active one and the anchor
# to their previous selection state.
#
# Arguments:
# w -		The listbox widget.

proc tkListboxCancel w {
    global tkPriv
    set q [winfo command $w]
    if {[$q cget -selectmode] != "extended"} {
	return
    }
    set first [$q index anchor]
    set last $tkPriv(listboxPrev)
    if {$first > $last} {
	set tmp $first
	set first $last
	set last $tmp
    }
    $q selection clear $first $last
    while {$first <= $last} {
	if {[lsearch $tkPriv(listboxSelection) $first] >= 0} {
	    $q selection set $first
	}
	incr first
    }
}

# tkListboxSelectAll
#
# This procedure is invoked to handle the "select all" operation.
# For single and browse mode, it just selects the active element.
# Otherwise it selects everything in the widget.
#
# Arguments:
# w -		The listbox widget.

proc tkListboxSelectAll w {
    set q [winfo command $w]
    set mode [$q cget -selectmode]
    if {($mode == "single") || ($mode == "browse")} {
	$q selection clear 0 end
	$q selection set active
    } else {
	$q selection set 0 end
    }
}

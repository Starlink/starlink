# entry.tcl --
#
# This file defines the default bindings for Tk entry widgets and provides
# procedures that help in implementing those bindings.
#
# SCCS: @(#) entry.tcl 1.43 96/08/23 14:07:15
#
# Copyright (c) 1992-1994 The Regents of the University of California.
# Copyright (c) 1994-1996 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

#-------------------------------------------------------------------------
# Elements of tkPriv that are used in this file:
#
# afterId -		If non-null, it means that auto-scanning is underway
#			and it gives the "after" id for the next auto-scan
#			command to be executed.
# mouseMoved -		Non-zero means the mouse has moved a significant
#			amount since the button went down (so, for example,
#			start dragging out a selection).
# pressX -		X-coordinate at which the mouse button was pressed.
# selectMode -		The style of selection currently underway:
#			char, word, or line.
# x, y -		Last known mouse coordinates for scanning
#			and auto-scanning.
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# The code below creates the default class bindings for entries.
#-------------------------------------------------------------------------

bind Entry <<Cut>> {
    if {[selection own -displayof %W] == "%W"} {
	clipboard clear -displayof %W
	catch {
	    clipboard append -displayof %W [selection get -displayof %W]
	    %q delete sel.first sel.last
	}
    }
}
bind Entry <<Copy>> {
    if {[selection own -displayof %W] == "%W"} {
	clipboard clear -displayof %W
	catch {
	    clipboard append -displayof %W [selection get -displayof %W]
	}
    }
}
bind Entry <<Paste>> {
    catch {
	%q insert insert [selection get -displayof %W -selection CLIPBOARD]
	tkEntrySeeInsert %W
    }
}
bind Entry <<Clear>> {
    %q delete sel.first sel.last
}

# Standard Motif bindings:

bind Entry <1> {
    tkEntryButton1 %W %x
    %q selection clear
}
bind Entry <B1-Motion> {
    set tkPriv(x) %x
    tkEntryMouseSelect %W %x
}
bind Entry <Double-1> {
    set tkPriv(selectMode) word
    tkEntryMouseSelect %W %x
    catch {%q icursor sel.first}
}
bind Entry <Triple-1> {
    set tkPriv(selectMode) line
    tkEntryMouseSelect %W %x
    %q icursor 0
}
bind Entry <Shift-1> {
    set tkPriv(selectMode) char
    %q selection adjust @%x
}
bind Entry <Double-Shift-1>	{
    set tkPriv(selectMode) word
    tkEntryMouseSelect %W %x
}
bind Entry <Triple-Shift-1>	{
    set tkPriv(selectMode) line
    tkEntryMouseSelect %W %x
}
bind Entry <B1-Leave> {
    set tkPriv(x) %x
    tkEntryAutoScan %W
}
bind Entry <B1-Enter> {
    tkCancelRepeat
}
bind Entry <ButtonRelease-1> {
    tkCancelRepeat
}
bind Entry <Control-1> {
    %q icursor @%x
}
bind Entry <ButtonRelease-2> {
    if {!$tkPriv(mouseMoved) || $tk_strictMotif} {
	tkEntryPaste %W %x
    }
}

bind Entry <Left> {
    tkEntrySetCursor %W [expr [%q index insert] - 1]
}
bind Entry <Right> {
    tkEntrySetCursor %W [expr [%q index insert] + 1]
}
bind Entry <Shift-Left> {
    tkEntryKeySelect %W [expr [%q index insert] - 1]
    tkEntrySeeInsert %W
}
bind Entry <Shift-Right> {
    tkEntryKeySelect %W [expr [%q index insert] + 1]
    tkEntrySeeInsert %W
}
bind Entry <Control-Left> {
    tkEntrySetCursor %W \
	    [string wordstart [%q get] [expr [%q index insert] - 1]]
}
bind Entry <Control-Right> {
    tkEntrySetCursor %W [string wordend [%q get] [%q index insert]]
}
bind Entry <Shift-Control-Left> {
    tkEntryKeySelect %W \
	    [string wordstart [%q get] [expr [%q index insert] - 1]]
    tkEntrySeeInsert %W
}
bind Entry <Shift-Control-Right> {
    tkEntryKeySelect %W [string wordend [%q get] [%q index insert]]
    tkEntrySeeInsert %W
}
bind Entry <Home> {
    tkEntrySetCursor %W 0
}
bind Entry <Shift-Home> {
    tkEntryKeySelect %W 0
    tkEntrySeeInsert %W
}
bind Entry <End> {
    tkEntrySetCursor %W end
}
bind Entry <Shift-End> {
    tkEntryKeySelect %W end
    tkEntrySeeInsert %W
}

bind Entry <Delete> {
    if [%q selection present] {
	%q delete sel.first sel.last
    } else {
	%q delete insert
    }
}
bind Entry <BackSpace> {
    tkEntryBackspace %W
}

bind Entry <Control-space> {
    %q selection from insert
}
bind Entry <Select> {
    %q selection from insert
}
bind Entry <Control-Shift-space> {
    %q selection adjust insert
}
bind Entry <Shift-Select> {
    %q selection adjust insert
}
bind Entry <Control-slash> {
    %q selection range 0 end
}
bind Entry <Control-backslash> {
    %q selection clear
}

bind Entry <KeyPress> {
    tkEntryInsert %W %A
}

# Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
# Otherwise, if a widget binding for one of these is defined, the
# <KeyPress> class binding will also fire and insert the character,
# which is wrong.  Ditto for Escape, Return, and Tab.

bind Entry <Alt-KeyPress> {# nothing}
bind Entry <Meta-KeyPress> {# nothing}
bind Entry <Control-KeyPress> {# nothing}
bind Entry <Escape> {# nothing}
bind Entry <Return> {# nothing}
bind Entry <KP_Enter> {# nothing}
bind Entry <Tab> {# nothing}

bind Entry <Insert> {
    catch {tkEntryInsert %W [selection get -displayof %W]}
}

# Additional emacs-like bindings:

bind Entry <Control-a> {
    if !$tk_strictMotif {
	tkEntrySetCursor %W 0
    }
}
bind Entry <Control-b> {
    if !$tk_strictMotif {
	tkEntrySetCursor %W [expr [%q index insert] - 1]
    }
}
bind Entry <Control-d> {
    if !$tk_strictMotif {
	%q delete insert
    }
}
bind Entry <Control-e> {
    if !$tk_strictMotif {
	tkEntrySetCursor %W end
    }
}
bind Entry <Control-f> {
    if !$tk_strictMotif {
	tkEntrySetCursor %W [expr [%q index insert] + 1]
    }
}
bind Entry <Control-h> {
    if !$tk_strictMotif {
	tkEntryBackspace %W
    }
}
bind Entry <Control-k> {
    if !$tk_strictMotif {
	%q delete insert end
    }
}
bind Entry <Control-t> {
    if !$tk_strictMotif {
	tkEntryTranspose %W
    }
}
bind Entry <Meta-b> {
    if !$tk_strictMotif {
	tkEntrySetCursor %W \
		[string wordstart [%q get] [expr [%q index insert] - 1]]
    }
}
bind Entry <Meta-d> {
    if !$tk_strictMotif {
	%q delete insert [string wordend [%q get] [%q index insert]]
    }
}
bind Entry <Meta-f> {
    if !$tk_strictMotif {
	tkEntrySetCursor %W [string wordend [%q get] [%q index insert]]
    }
}
bind Entry <Meta-BackSpace> {
    if !$tk_strictMotif {
	%q delete [string wordstart [%q get] [expr [%q index insert] - 1]] \
		insert
    }
}

# A few additional bindings of my own.

bind Entry <2> {
    if !$tk_strictMotif {
	%q scan mark %x
	set tkPriv(x) %x
	set tkPriv(y) %y
	set tkPriv(mouseMoved) 0
    }
}
bind Entry <B2-Motion> {
    if !$tk_strictMotif {
	if {abs(%x-$tkPriv(x)) > 2} {
	    set tkPriv(mouseMoved) 1
	}
	%q scan dragto %x
    }
}

# tkEntryClosestGap --
# Given x and y coordinates, this procedure finds the closest boundary
# between characters to the given coordinates and returns the index
# of the character just after the boundary.
#
# Arguments:
# w -		The entry window.
# x -		X-coordinate within the window.

proc tkEntryClosestGap {w x} {
    set q [winfo command $w]
    set pos [$q index @$x]
    set bbox [$q bbox $pos]
    if {($x - [lindex $bbox 0]) < ([lindex $bbox 2]/2)} {
	return $pos
    }
    incr pos
}

# tkEntryButton1 --
# This procedure is invoked to handle button-1 presses in entry
# widgets.  It moves the insertion cursor, sets the selection anchor,
# and claims the input focus.
#
# Arguments:
# w -		The entry window in which the button was pressed.
# x -		The x-coordinate of the button press.

proc tkEntryButton1 {w x} {
    global tkPriv

    set tkPriv(selectMode) char
    set tkPriv(mouseMoved) 0
    set tkPriv(pressX) $x
    set q [winfo command $w]
    $q icursor [tkEntryClosestGap $w $x]
    $q selection from insert
    if {[lindex [$q configure -state] 4] == "normal"} {focus $w}
}

# tkEntryMouseSelect --
# This procedure is invoked when dragging out a selection with
# the mouse.  Depending on the selection mode (character, word,
# line) it selects in different-sized units.  This procedure
# ignores mouse motions initially until the mouse has moved from
# one character to another or until there have been multiple clicks.
#
# Arguments:
# w -		The entry window in which the button was pressed.
# x -		The x-coordinate of the mouse.

proc tkEntryMouseSelect {w x} {
    global tkPriv

    set q [winfo command $w]
    set cur [tkEntryClosestGap $w $x]
    set anchor [$q index anchor]
    if {($cur != $anchor) || (abs($tkPriv(pressX) - $x) >= 3)} {
	set tkPriv(mouseMoved) 1
    }
    switch $tkPriv(selectMode) {
	char {
	    if $tkPriv(mouseMoved) {
		if {$cur < $anchor} {
		    $q selection range $cur $anchor
		} elseif {$cur > $anchor} {
		    $q selection range $anchor $cur
		} else {
		    $q selection clear
		}
	    }
	}
	word {
	    if {$cur < [$q index anchor]} {
		$q selection range [string wordstart [$q get] $cur] \
			[string wordend [$q get] [expr $anchor-1]]
	    } else {
		$q selection range [string wordstart [$q get] $anchor] \
			[string wordend [$q get] [expr $cur - 1]]
	    }
	}
	line {
	    $q selection range 0 end
	}
    }
    update idletasks
}

# tkEntryPaste --
# This procedure sets the insertion cursor to the current mouse position,
# pastes the selection there, and sets the focus to the window.
#
# Arguments:
# w -		The entry window.
# x -		X position of the mouse.

proc tkEntryPaste {w x} {
    global tkPriv

    set q [winfo command $w]
    $q icursor [tkEntryClosestGap $w $x]
    catch {$q insert insert [selection get -displayof $w]}
    if {[lindex [$q configure -state] 4] == "normal"} {focus $w}
}

# tkEntryAutoScan --
# This procedure is invoked when the mouse leaves an entry window
# with button 1 down.  It scrolls the window left or right,
# depending on where the mouse is, and reschedules itself as an
# "after" command so that the window continues to scroll until the
# mouse moves back into the window or the mouse button is released.
#
# Arguments:
# w -		The entry window.

proc tkEntryAutoScan {w} {
    global tkPriv
    set q [winfo command $w]
    set x $tkPriv(x)
    if {![winfo exists $w]} return
    if {$x >= [winfo width $w]} {
	$q xview scroll 2 units
	tkEntryMouseSelect $w $x
    } elseif {$x < 0} {
	$q xview scroll -2 units
	tkEntryMouseSelect $w $x
    }
    set tkPriv(afterId) [after 50 tkEntryAutoScan $w]
}

# tkEntryKeySelect --
# This procedure is invoked when stroking out selections using the
# keyboard.  It moves the cursor to a new position, then extends
# the selection to that position.
#
# Arguments:
# w -		The entry window.
# new -		A new position for the insertion cursor (the cursor hasn't
#		actually been moved to this position yet).

proc tkEntryKeySelect {w new} {
    set q [winfo command $w]
    if ![$q selection present] {
	$q selection from insert
	$q selection to $new
    } else {
	$q selection adjust $new
    }
    $q icursor $new
}

# tkEntryInsert --
# Insert a string into an entry at the point of the insertion cursor.
# If there is a selection in the entry, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w -		The entry window in which to insert the string
# s -		The string to insert (usually just a single character)

proc tkEntryInsert {w s} {
    if {$s == ""} {
	return
    }
    set q [winfo command $w]
    catch {
	set insert [$q index insert]
	if {([$q index sel.first] <= $insert)
		&& ([$q index sel.last] >= $insert)} {
	    $q delete sel.first sel.last
	}
    }
    $q insert insert $s
    tkEntrySeeInsert $w
}

# tkEntryBackspace --
# Backspace over the character just before the insertion cursor.
# If backspacing would move the cursor off the left edge of the
# window, reposition the cursor at about the middle of the window.
#
# Arguments:
# w -		The entry window in which to backspace.

proc tkEntryBackspace w {
    set q [winfo command $w]
    if [$q selection present] {
	$q delete sel.first sel.last
    } else {
	set x [expr {[$q index insert] - 1}]
	if {$x >= 0} {$q delete $x}
	if {[$q index @0] >= [$q index insert]} {
	    set range [$q xview]
	    set left [lindex $range 0]
	    set right [lindex $range 1]
	    $q xview moveto [expr $left - ($right - $left)/2.0]
	}
    }
}

# tkEntrySeeInsert --
# Make sure that the insertion cursor is visible in the entry window.
# If not, adjust the view so that it is.
#
# Arguments:
# w -		The entry window.

proc tkEntrySeeInsert w {
    set q [winfo command $w]
    set c [$q index insert]
    set left [$q index @0]
    if {$left > $c} {
	$q xview $c
	return
    }
    set x [winfo width $w]
    while {([$q index @$x] <= $c) && ($left < $c)} {
	incr left
	$q xview $left
    }
}

# tkEntrySetCursor -
# Move the insertion cursor to a given position in an entry.  Also
# clears the selection, if there is one in the entry, and makes sure
# that the insertion cursor is visible.
#
# Arguments:
# w -		The entry window.
# pos -		The desired new position for the cursor in the window.

proc tkEntrySetCursor {w pos} {
    set q [winfo command $w]
    $q icursor $pos
    $q selection clear
    tkEntrySeeInsert $w
}

# tkEntryTranspose -
# This procedure implements the "transpose" function for entry widgets.
# It tranposes the characters on either side of the insertion cursor,
# unless the cursor is at the end of the line.  In this case it
# transposes the two characters to the left of the cursor.  In either
# case, the cursor ends up to the right of the transposed characters.
#
# Arguments:
# w -		The entry window.

proc tkEntryTranspose w {
    set q [winfo command $w]
    set i [$q index insert]
    if {$i < [$q index end]} {
	incr i
    }
    set first [expr $i-2]
    if {$first < 0} {
	return
    }
    set new [string index [$q get] [expr $i-1]][string index [$q get] $first]
    $q delete $first $i
    $q insert insert $new
    tkEntrySeeInsert $w
}

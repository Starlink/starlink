# text.tcl --
#
# This file defines the default bindings for Tk text widgets and provides
# procedures that help in implementing the bindings.
#
# SCCS: @(#) text.tcl 1.46 96/08/23 14:07:32
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
# char -		Character position on the line;  kept in order
#			to allow moving up or down past short lines while
#			still remembering the desired position.
# mouseMoved -		Non-zero means the mouse has moved a significant
#			amount since the button went down (so, for example,
#			start dragging out a selection).
# prevPos -		Used when moving up or down lines via the keyboard.
#			Keeps track of the previous insert position, so
#			we can distinguish a series of ups and downs, all
#			in a row, from a new up or down.
# selectMode -		The style of selection currently underway:
#			char, word, or line.
# x, y -		Last known mouse coordinates for scanning
#			and auto-scanning.
#-------------------------------------------------------------------------

#-------------------------------------------------------------------------
# The code below creates the default class bindings for entries.
#-------------------------------------------------------------------------

# Standard Motif bindings:

bind Text <1> {
    tkTextButton1 %W %x %y
    %q tag remove sel 0.0 end
}
bind Text <B1-Motion> {
    set tkPriv(x) %x
    set tkPriv(y) %y
    tkTextSelectTo %W %x %y
}
bind Text <Double-1> {
    set tkPriv(selectMode) word
    tkTextSelectTo %W %x %y
    catch {%q mark set insert sel.first}
}
bind Text <Triple-1> {
    set tkPriv(selectMode) line
    tkTextSelectTo %W %x %y
    catch {%q mark set insert sel.first}
}
bind Text <Shift-1> {
    tkTextResetAnchor %W @%x,%y
    set tkPriv(selectMode) char
    tkTextSelectTo %W %x %y
}
bind Text <Double-Shift-1>	{
    set tkPriv(selectMode) word
    tkTextSelectTo %W %x %y
}
bind Text <Triple-Shift-1>	{
    set tkPriv(selectMode) line
    tkTextSelectTo %W %x %y
}
bind Text <B1-Leave> {
    set tkPriv(x) %x
    set tkPriv(y) %y
    tkTextAutoScan %W
}
bind Text <B1-Enter> {
    tkCancelRepeat
}
bind Text <ButtonRelease-1> {
    tkCancelRepeat
}
bind Text <Control-1> {
    %q mark set insert @%x,%y
}
bind Text <ButtonRelease-2> {
    if {!$tkPriv(mouseMoved) || $tk_strictMotif} {
	tkTextPaste %W %x %y
    }
}
bind Text <Left> {
    tkTextSetCursor %W insert-1c
}
bind Text <Right> {
    tkTextSetCursor %W insert+1c
}
bind Text <Up> {
    tkTextSetCursor %W [tkTextUpDownLine %W -1]
}
bind Text <Down> {
    tkTextSetCursor %W [tkTextUpDownLine %W 1]
}
bind Text <Shift-Left> {
    tkTextKeySelect %W [%q index {insert - 1c}]
}
bind Text <Shift-Right> {
    tkTextKeySelect %W [%q index {insert + 1c}]
}
bind Text <Shift-Up> {
    tkTextKeySelect %W [tkTextUpDownLine %W -1]
}
bind Text <Shift-Down> {
    tkTextKeySelect %W [tkTextUpDownLine %W 1]
}
bind Text <Control-Left> {
    tkTextSetCursor %W [%q index {insert - 1c wordstart}]
}
bind Text <Control-Right> {
    tkTextSetCursor %W [%q index {insert wordend}]
}
bind Text <Control-Up> {
    tkTextSetCursor %W [tkTextPrevPara %W insert]
}
bind Text <Control-Down> {
    tkTextSetCursor %W [tkTextNextPara %W insert]
}
bind Text <Shift-Control-Left> {
    tkTextKeySelect %W [%q index {insert - 1c wordstart}]
}
bind Text <Shift-Control-Right> {
    tkTextKeySelect %W [%q index {insert wordend}]
}
bind Text <Shift-Control-Up> {
    tkTextKeySelect %W [tkTextPrevPara %W insert]
}
bind Text <Shift-Control-Down> {
    tkTextKeySelect %W [tkTextNextPara %W insert]
}
bind Text <Prior> {
    tkTextSetCursor %W [tkTextScrollPages %W -1]
}
bind Text <Shift-Prior> {
    tkTextKeySelect %W [tkTextScrollPages %W -1]
}
bind Text <Next> {
    tkTextSetCursor %W [tkTextScrollPages %W 1]
}
bind Text <Shift-Next> {
    tkTextKeySelect %W [tkTextScrollPages %W 1]
}
bind Text <Control-Prior> {
    %q xview scroll -1 page
}
bind Text <Control-Next> {
    %q xview scroll 1 page
}

bind Text <Home> {
    tkTextSetCursor %W {insert linestart}
}
bind Text <Shift-Home> {
    tkTextKeySelect %W {insert linestart}
}
bind Text <End> {
    tkTextSetCursor %W {insert lineend}
}
bind Text <Shift-End> {
    tkTextKeySelect %W {insert lineend}
}
bind Text <Control-Home> {
    tkTextSetCursor %W 1.0
}
bind Text <Control-Shift-Home> {
    tkTextKeySelect %W 1.0
}
bind Text <Control-End> {
    tkTextSetCursor %W {end - 1 char}
}
bind Text <Control-Shift-End> {
    tkTextKeySelect %W {end - 1 char}
}

bind Text <Tab> {
    tkTextInsert %W \t
    focus %W
    break
}
bind Text <Shift-Tab> {
    # Needed only to keep <Tab> binding from triggering;  doesn't
    # have to actually do anything.
}
bind Text <Control-Tab> {
    focus [tk_focusNext %W]
}
bind Text <Control-Shift-Tab> {
    focus [tk_focusPrev %W]
}
bind Text <Control-i> {
    tkTextInsert %W \t
}
bind Text <Return> {
    tkTextInsert %W \n
}
bind Text <Delete> {
    if {[%q tag nextrange sel 1.0 end] != ""} {
	%q delete sel.first sel.last
    } else {
	%q delete insert
	%q see insert
    }
}
bind Text <BackSpace> {
    if {[%q tag nextrange sel 1.0 end] != ""} {
	%q delete sel.first sel.last
    } elseif [%q compare insert != 1.0] {
	%q delete insert-1c
	%q see insert
    }
}

bind Text <Control-space> {
    %q mark set anchor insert
}
bind Text <Select> {
    %q mark set anchor insert
}
bind Text <Control-Shift-space> {
    set tkPriv(selectMode) char
    tkTextKeyExtend %W insert
}
bind Text <Shift-Select> {
    set tkPriv(selectMode) char
    tkTextKeyExtend %W insert
}
bind Text <Control-slash> {
    %q tag add sel 1.0 end
}
bind Text <Control-backslash> {
    %q tag remove sel 1.0 end
}
bind Text <<Cut>> {
    tk_textCut %W
}
bind Text <<Copy>> {
    tk_textCopy %W
}
bind Text <<Paste>> {
    tk_textPaste %W
}
bind Text <<Clear>> {
    %q delete sel.first sel.last
}
bind Text <Insert> {
    catch {tkTextInsert %W [selection get -displayof %W]}
}
bind Text <KeyPress> {
    tkTextInsert %W %A
}

# Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
# Otherwise, if a widget binding for one of these is defined, the
# <KeyPress> class binding will also fire and insert the character,
# which is wrong.  Ditto for <Escape>.

bind Text <Alt-KeyPress> {# nothing }
bind Text <Meta-KeyPress> {# nothing}
bind Text <Control-KeyPress> {# nothing}
bind Text <Escape> {# nothing}
bind Text <KP_Enter> {# nothing}

# Additional emacs-like bindings:

bind Text <Control-a> {
    if !$tk_strictMotif {
	tkTextSetCursor %W {insert linestart}
    }
}
bind Text <Control-b> {
    if !$tk_strictMotif {
	tkTextSetCursor %W insert-1c
    }
}
bind Text <Control-d> {
    if !$tk_strictMotif {
	%q delete insert
    }
}
bind Text <Control-e> {
    if !$tk_strictMotif {
	tkTextSetCursor %W {insert lineend}
    }
}
bind Text <Control-f> {
    if !$tk_strictMotif {
	tkTextSetCursor %W insert+1c
    }
}
bind Text <Control-k> {
    if !$tk_strictMotif {
	if [%q compare insert == {insert lineend}] {
	    %q delete insert
	} else {
	    %q delete insert {insert lineend}
	}
    }
}
bind Text <Control-n> {
    if !$tk_strictMotif {
	tkTextSetCursor %W [tkTextUpDownLine %W 1]
    }
}
bind Text <Control-o> {
    if !$tk_strictMotif {
	%q insert insert \n
	%q mark set insert insert-1c
    }
}
bind Text <Control-p> {
    if !$tk_strictMotif {
	tkTextSetCursor %W [tkTextUpDownLine %W -1]
    }
}
bind Text <Control-t> {
    if !$tk_strictMotif {
	tkTextTranspose %W
    }
}
bind Text <Control-v> {
    if !$tk_strictMotif {
	tkTextScrollPages %W 1
    }
}
bind Text <Meta-b> {
    if !$tk_strictMotif {
	tkTextSetCursor %W {insert - 1c wordstart}
    }
}
bind Text <Meta-d> {
    if !$tk_strictMotif {
	%q delete insert {insert wordend}
    }
}
bind Text <Meta-f> {
    if !$tk_strictMotif {
	tkTextSetCursor %W {insert wordend}
    }
}
bind Text <Meta-less> {
    if !$tk_strictMotif {
	tkTextSetCursor %W 1.0
    }
}
bind Text <Meta-greater> {
    if !$tk_strictMotif {
	tkTextSetCursor %W end-1c
    }
}
bind Text <Meta-BackSpace> {
    if !$tk_strictMotif {
	%q delete {insert -1c wordstart} insert
    }
}
bind Text <Meta-Delete> {
    if !$tk_strictMotif {
	%q delete {insert -1c wordstart} insert
    }
}

# A few additional bindings of my own.

bind Text <Control-h> {
    if !$tk_strictMotif {
	if [%q compare insert != 1.0] {
	    %q delete insert-1c
	    %q see insert
	}
    }
}
bind Text <2> {
    if !$tk_strictMotif {
	%q scan mark %x %y
	set tkPriv(x) %x
	set tkPriv(y) %y
	set tkPriv(mouseMoved) 0
    }
}
bind Text <B2-Motion> {
    if !$tk_strictMotif {
	if {(%x != $tkPriv(x)) || (%y != $tkPriv(y))} {
	    set tkPriv(mouseMoved) 1
	}
	if $tkPriv(mouseMoved) {
	    %q scan dragto %x %y
	}
    }
}
set tkPriv(prevPos) {}

# tkTextClosestGap --
# Given x and y coordinates, this procedure finds the closest boundary
# between characters to the given coordinates and returns the index
# of the character just after the boundary.
#
# Arguments:
# w -		The text window.
# x -		X-coordinate within the window.
# y -		Y-coordinate within the window.

proc tkTextClosestGap {w x y} {
    set q [winfo command $w]
    set pos [$q index @$x,$y]
    set bbox [$q bbox $pos]
    if ![string compare $bbox ""] {
	return $pos
    }
    if {($x - [lindex $bbox 0]) < ([lindex $bbox 2]/2)} {
	return $pos
    }
    $q index "$pos + 1 char"
}

# tkTextButton1 --
# This procedure is invoked to handle button-1 presses in text
# widgets.  It moves the insertion cursor, sets the selection anchor,
# and claims the input focus.
#
# Arguments:
# w -		The text window in which the button was pressed.
# x -		The x-coordinate of the button press.
# y -		The x-coordinate of the button press.

proc tkTextButton1 {w x y} {
    global tkPriv

    set q [winfo command $w]
    set tkPriv(selectMode) char
    set tkPriv(mouseMoved) 0
    set tkPriv(pressX) $x
    $q mark set insert [tkTextClosestGap $w $x $y]
    $q mark set anchor insert
    if {[$q cget -state] == "normal"} {focus $w}
}

# tkTextSelectTo --
# This procedure is invoked to extend the selection, typically when
# dragging it with the mouse.  Depending on the selection mode (character,
# word, line) it selects in different-sized units.  This procedure
# ignores mouse motions initially until the mouse has moved from
# one character to another or until there have been multiple clicks.
#
# Arguments:
# w -		The text window in which the button was pressed.
# x -		Mouse x position.
# y - 		Mouse y position.

proc tkTextSelectTo {w x y} {
    global tkPriv

    set q [winfo command $w]
    set cur [tkTextClosestGap $w $x $y]
    if [catch {$q index anchor}] {
	$q mark set anchor $cur
    }
    set anchor [$q index anchor]
    if {[$q compare $cur != $anchor] || (abs($tkPriv(pressX) - $x) >= 3)} {
	set tkPriv(mouseMoved) 1
    }
    switch $tkPriv(selectMode) {
	char {
	    if [$q compare $cur < anchor] {
		set first $cur
		set last anchor
	    } else {
		set first anchor
		set last $cur
	    }
	}
	word {
	    if [$q compare $cur < anchor] {
		set first [$q index "$cur wordstart"]
		set last [$q index "anchor - 1c wordend"]
	    } else {
		set first [$q index "anchor wordstart"]
		set last [$q index "$cur -1c wordend"]
	    }
	}
	line {
	    if [$q compare $cur < anchor] {
		set first [$q index "$cur linestart"]
		set last [$q index "anchor - 1c lineend + 1c"]
	    } else {
		set first [$q index "anchor linestart"]
		set last [$q index "$cur lineend + 1c"]
	    }
	}
    }
    if {$tkPriv(mouseMoved) || ($tkPriv(selectMode) != "char")} {
	$q tag remove sel 0.0 $first
	$q tag add sel $first $last
	$q tag remove sel $last end
	update idletasks
    }
}

# tkTextKeyExtend --
# This procedure handles extending the selection from the keyboard,
# where the point to extend to is really the boundary between two
# characters rather than a particular character.
#
# Arguments:
# w -		The text window.
# index -	The point to which the selection is to be extended.

proc tkTextKeyExtend {w index} {
    global tkPriv

    set q [winfo command $w]
    set cur [$q index $index]
    if [catch {$q index anchor}] {
	$q mark set anchor $cur
    }
    set anchor [$q index anchor]
    if [$q compare $cur < anchor] {
	set first $cur
	set last anchor
    } else {
	set first anchor
	set last $cur
    }
    $q tag remove sel 0.0 $first
    $q tag add sel $first $last
    $q tag remove sel $last end
}

# tkTextPaste --
# This procedure sets the insertion cursor to the mouse position,
# inserts the selection, and sets the focus to the window.
#
# Arguments:
# w -		The text window.
# x, y - 	Position of the mouse.

proc tkTextPaste {w x y} {
    set q [winfo command $w]
    $q mark set insert [tkTextClosestGap $w $x $y]
    catch {$q insert insert [selection get -displayof $w]}
    if {[$q cget -state] == "normal"} {focus $w}
}

# tkTextAutoScan --
# This procedure is invoked when the mouse leaves a text window
# with button 1 down.  It scrolls the window up, down, left, or right,
# depending on where the mouse is (this information was saved in
# tkPriv(x) and tkPriv(y)), and reschedules itself as an "after"
# command so that the window continues to scroll until the mouse
# moves back into the window or the mouse button is released.
#
# Arguments:
# w -		The text window.

proc tkTextAutoScan {w} {
    global tkPriv
    if {![winfo exists $w]} return
    set q [winfo command $w]
    if {$tkPriv(y) >= [winfo height $w]} {
	$q yview scroll 2 units
    } elseif {$tkPriv(y) < 0} {
	$q yview scroll -2 units
    } elseif {$tkPriv(x) >= [winfo width $w]} {
	$q xview scroll 2 units
    } elseif {$tkPriv(x) < 0} {
	$q xview scroll -2 units
    } else {
	return
    }
    tkTextSelectTo $w $tkPriv(x) $tkPriv(y)
    set tkPriv(afterId) [after 50 tkTextAutoScan $w]
}

# tkTextSetCursor
# Move the insertion cursor to a given position in a text.  Also
# clears the selection, if there is one in the text, and makes sure
# that the insertion cursor is visible.  Also, don't let the insertion
# cursor appear on the dummy last line of the text.
#
# Arguments:
# w -		The text window.
# pos -		The desired new position for the cursor in the window.

proc tkTextSetCursor {w pos} {
    global tkPriv

    set q [winfo command $w]
    if [$q compare $pos == end] {
	set pos {end - 1 chars}
    }
    $q mark set insert $pos
    $q tag remove sel 1.0 end
    $q see insert
}

# tkTextKeySelect
# This procedure is invoked when stroking out selections using the
# keyboard.  It moves the cursor to a new position, then extends
# the selection to that position.
#
# Arguments:
# w -		The text window.
# new -		A new position for the insertion cursor (the cursor hasn't
#		actually been moved to this position yet).

proc tkTextKeySelect {w new} {
    global tkPriv

    set q [winfo command $w]
    if {[$q tag nextrange sel 1.0 end] == ""} {
	if [$q compare $new < insert] {
	    $q tag add sel $new insert
	} else {
	    $q tag add sel insert $new
	}
	$q mark set anchor insert
    } else {
	if [$q compare $new < anchor] {
	    set first $new
	    set last anchor
	} else {
	    set first anchor
	    set last $new
	}
	$q tag remove sel 1.0 $first
	$q tag add sel $first $last
	$q tag remove sel $last end
    }
    $q mark set insert $new
    $q see insert
    update idletasks
}

# tkTextResetAnchor --
# Set the selection anchor to whichever end is farthest from the
# index argument.  One special trick: if the selection has two or
# fewer characters, just leave the anchor where it is.  In this
# case it doesn't matter which point gets chosen for the anchor,
# and for the things like Shift-Left and Shift-Right this produces
# better behavior when the cursor moves back and forth across the
# anchor.
#
# Arguments:
# w -		The text widget.
# index -	Position at which mouse button was pressed, which determines
#		which end of selection should be used as anchor point.

proc tkTextResetAnchor {w index} {
    global tkPriv

    set q [winfo command $w]
    if {[$q tag ranges sel] == ""} {
	$q mark set anchor $index
	return
    }
    set a [$q index $index]
    set b [$q index sel.first]
    set c [$q index sel.last]
    if [$q compare $a < $b] {
	$q mark set anchor sel.last
	return
    }
    if [$q compare $a > $c] {
	$q mark set anchor sel.first
	return
    }
    scan $a "%d.%d" lineA chA
    scan $b "%d.%d" lineB chB
    scan $c "%d.%d" lineC chC
    if {$lineB < $lineC+2} {
	set total [string length [$q get $b $c]]
	if {$total <= 2} {
	    return
	}
	if {[string length [$q get $b $a]] < ($total/2)} {
	    $q mark set anchor sel.last
	} else {
	    $q mark set anchor sel.first
	}
	return
    }
    if {($lineA-$lineB) < ($lineC-$lineA)} {
	$q mark set anchor sel.last
    } else {
	$q mark set anchor sel.first
    }
}

# tkTextInsert --
# Insert a string into a text at the point of the insertion cursor.
# If there is a selection in the text, and it covers the point of the
# insertion cursor, then delete the selection before inserting.
#
# Arguments:
# w -		The text window in which to insert the string
# s -		The string to insert (usually just a single character)

proc tkTextInsert {w s} {
    set q [winfo command $w]
    if {($s == "") || ([$q cget -state] == "disabled")} {
	return
    }
    catch {
	if {[$q compare sel.first <= insert]
		&& [$q compare sel.last >= insert]} {
	    $q delete sel.first sel.last
	}
    }
    $q insert insert $s
    $q see insert
}

# tkTextUpDownLine --
# Returns the index of the character one line above or below the
# insertion cursor.  There are two tricky things here.  First,
# we want to maintain the original column across repeated operations,
# even though some lines that will get passed through don't have
# enough characters to cover the original column.  Second, don't
# try to scroll past the beginning or end of the text.
#
# Arguments:
# w -		The text window in which the cursor is to move.
# n -		The number of lines to move: -1 for up one line,
#		+1 for down one line.

proc tkTextUpDownLine {w n} {
    global tkPriv

    set q [winfo command $w]
    set i [$q index insert]
    scan $i "%d.%d" line char
    if {[string compare $tkPriv(prevPos) $i] != 0} {
	set tkPriv(char) $char
    }
    set new [$q index [expr $line + $n].$tkPriv(char)]
    if {[$q compare $new == end] || [$q compare $new == "insert linestart"]} {
	set new $i
    }
    set tkPriv(prevPos) $new
    return $new
}

# tkTextPrevPara --
# Returns the index of the beginning of the paragraph just before a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w -		The text window in which the cursor is to move.
# pos -		Position at which to start search.

proc tkTextPrevPara {w pos} {
    set q [winfo command $w]
    set pos [$q index "$pos linestart"]
    while 1 {
	if {(([$q get "$pos - 1 line"] == "\n") && ([$q get $pos] != "\n"))
		|| ($pos == "1.0")} {
	    if [regexp -indices {^[ 	]+(.)} [$q get $pos "$pos lineend"] \
		    dummy index] {
		set pos [$q index "$pos + [lindex $index 0] chars"]
	    }
	    if {[$q compare $pos != insert] || ($pos == "1.0")} {
		return $pos
	    }
	}
	set pos [$q index "$pos - 1 line"]
    }
}

# tkTextNextPara --
# Returns the index of the beginning of the paragraph just after a given
# position in the text (the beginning of a paragraph is the first non-blank
# character after a blank line).
#
# Arguments:
# w -		The text window in which the cursor is to move.
# start -	Position at which to start search.

proc tkTextNextPara {w start} {
    set q [winfo command $w]
    set pos [$q index "$start linestart + 1 line"]
    while {[$q get $pos] != "\n"} {
	if [$q compare $pos == end] {
	    return [$q index "end - 1c"]
	}
	set pos [$q index "$pos + 1 line"]
    }
    while {[$q get $pos] == "\n"} {
	set pos [$q index "$pos + 1 line"]
	if [$q compare $pos == end] {
	    return [$q index "end - 1c"]
	}
    }
    if [regexp -indices {^[ 	]+(.)} [$q get $pos "$pos lineend"] \
	    dummy index] {
	return [$q index "$pos + [lindex $index 0] chars"]
    }
    return $pos
}

# tkTextScrollPages --
# This is a utility procedure used in bindings for moving up and down
# pages and possibly extending the selection along the way.  It scrolls
# the view in the widget by the number of pages, and it returns the
# index of the character that is at the same position in the new view
# as the insertion cursor used to be in the old view.
#
# Arguments:
# w -		The text window in which the cursor is to move.
# count -	Number of pages forward to scroll;  may be negative
#		to scroll backwards.

proc tkTextScrollPages {w count} {
    set q [winfo command $w]
    set bbox [$q bbox insert]
    $q yview scroll $count pages
    if {$bbox == ""} {
	return [$q index @[expr [winfo height $w]/2],0]
    }
    return [$q index @[lindex $bbox 0],[lindex $bbox 1]]
}

# tkTextTranspose --
# This procedure implements the "transpose" function for text widgets.
# It tranposes the characters on either side of the insertion cursor,
# unless the cursor is at the end of the line.  In this case it
# transposes the two characters to the left of the cursor.  In either
# case, the cursor ends up to the right of the transposed characters.
#
# Arguments:
# w -		Text window in which to transpose.

proc tkTextTranspose w {
    set q [winfo command $w]
    set pos insert
    if [$q compare $pos != "$pos lineend"] {
	set pos [$q index "$pos + 1 char"]
    }
    set new [$q get "$pos - 1 char"][$q get  "$pos - 2 char"]
    if [$q compare "$pos - 1 char" == 1.0] {
	return
    }
    $q delete "$pos - 2 char" $pos
    $q insert insert $new
    $q see insert
}

# tk_textCopy --
# This procedure copies the selection from a text widget into the
# clipboard.
#
# Arguments:
# w -		Name of a text widget.

proc tk_textCopy w {
    if {[selection own -displayof $w] == "$w"} {
	clipboard clear -displayof $w
	catch {
	    clipboard append -displayof $w [selection get -displayof $w]
	}
    }
}

# tk_textCut --
# This procedure copies the selection from a text widget into the
# clipboard, then deletes the selection (if it exists in the given
# widget).
#
# Arguments:
# w -		Name of a text widget.

proc tk_textCut w {
    set q [winfo command $w]
    if {[selection own -displayof $w] == "$w"} {
	clipboard clear -displayof $w
	catch {
	    clipboard append -displayof $w [selection get -displayof $w]
	    $q delete sel.first sel.last
	}
    }
}

# tk_textPaste --
# This procedure pastes the contents of the clipboard to the insertion
# point in a text widget.
#
# Arguments:
# w -		Name of a text widget.

proc tk_textPaste w {
    set q [winfo command $w]
    catch {
	$q insert insert [selection get -displayof $w \
		-selection CLIPBOARD]
    }
}

# bltHier.tcl
# ----------------------------------------------------------------------
# Bindings for the BLT hierbox widget
# ----------------------------------------------------------------------
#   AUTHOR:  George Howlett
#            Bell Labs Innovations for Lucent Technologies
#            gah@lucent.com
#            http://www.tcltk.com/blt
#
#      RCS:  $Id: bltHier.tcl,v 1.4 1998/08/26 23:38:37 gah Exp $
# ----------------------------------------------------------------------
# Copyright (c) 1998  Lucent Technologies, Inc.
# ======================================================================
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that the copyright notice and warranty disclaimer appear in
# supporting documentation, and that the names of Lucent Technologies
# any of their entities not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# Lucent Technologies disclaims all warranties with regard to this
# software, including all implied warranties of merchantability and
# fitness.  In no event shall Lucent be liable for any special, indirect
# or consequential damages or any damages whatsoever resulting from loss
# of use, data or profits, whether in an action of contract, negligence
# or other tortuous action, arising out of or in connection with the use
# or performance of this software.
# ======================================================================

array set bltHierbox {
    afterId ""
    space   off
    action  ""
    x       0
    y       0
    toggle  yes
    select  off
}

# 
# ButtonPress assignments
#
#	B1-Enter	start auto-scrolling
#	B1-Leave	stop auto-scrolling
#	ButtonPress-2	start scan
#	B2-Motion	adjust scan
#	ButtonRelease-2 stop scan

bind Hierbox <ButtonPress-2> {
    set bltHierbox(cursor) [%W cget -cursor]
    %W configure -cursor hand1
    %W scan mark %x %y
}

bind Hierbox <B2-Motion> {
    %W scan dragto %x %y
}

bind Hierbox <ButtonRelease-2> {
    %W configure -cursor $bltHierbox(cursor)
}

bind Hierbox <B1-Leave> {
    blt::HierboxAutoScan %W 
}

bind Hierbox <B1-Enter> {
    after cancel $bltHierbox(afterId)
}


# 
# KeyPress assignments
#
#	Up			
#	Down
#	Shift-Up
#	Shift-Down
#	Prior (PageUp)
#	Next  (PageDn)
#	Left
#	Right
#	space		Start selection toggle of entry currently with focus.
#	Return		Start selection toggle of entry currently with focus.
#	Home
#	End
#	F1
#	F2
#	ascii char	Go to next open entry starting with character.
#
# KeyRelease
#
#	space		Stop selection toggle of entry currently with focus.
#	Return		Stop selection toggle of entry currently with focus.

bind Hierbox <KeyPress-Up> {
    blt::HierboxTraverse %W up
    if { $bltHierbox(space) } {
	%W selection toggle focus
    }
}

bind Hierbox <KeyPress-Down> {
    blt::HierboxTraverse %W down
    if { $bltHierbox(space) } {
	%W selection toggle focus
    }
}

bind Hierbox <Shift-KeyPress-Up> {
    blt::HierboxTraverse %W prevsibling
}

bind Hierbox <Shift-KeyPress-Down> {
    blt::HierboxTraverse %W nextsibling
}

bind Hierbox <KeyPress-Prior> {
    blt::HierboxPage %W top
}

bind Hierbox <KeyPress-Next> {
    blt::HierboxPage %W bottom
}

bind Hierbox <KeyPress-Left> {
    %W close focus
}
bind Hierbox <KeyPress-Right> {
    %W open focus
}

bind Hierbox <KeyPress-space> {
    blt::HierboxToggle %W focus
    set bltHierbox(space) on
}

bind Hierbox <KeyRelease-space> { 
    set bltHierbox(space) off
}

bind Hierbox <KeyPress-Return> {
    blt::HierboxToggle %W focus
    set bltHierbox(space) on
}

bind Hierbox <KeyRelease-Return> { 
    set bltHierbox(space) off
}

bind Hierbox <KeyPress> {
    blt::HierboxSearch %W %A
}

bind Hierbox <KeyPress-Home> {
    blt::HierboxTraverse %W root
}

bind Hierbox <KeyPress-End> {
    blt::HierboxTraverse %W end
}

bind Hierbox <KeyPress-F1> {
    %W open -r root
}

bind Hierbox <KeyPress-F2> {
    eval %W close -r [%W entry children root 0 end] 
}


# ----------------------------------------------------------------------
# USAGE: blt::HierboxInitBindings <hierbox> 
#
# Invoked by internally by Hierbox_Init routine.  Initializes the 
# default bindings for the hierbox widget entries.  These are local
# to the widget, so they can't be set through the widget's class
# bind tags.
#
# Arguments:	hierbox		hierarchy widget
#
# ----------------------------------------------------------------------
proc blt::HierboxInitBindings { widget } {
    $widget bind all <Enter> { 
	%W entry activate current 
    }
    $widget bind all <Leave> { 
	%W entry activate "" 
    }
    $widget bind all <ButtonPress-1> { 	
	blt::HierboxSelect %W 
    }
    $widget bind all <Double-ButtonPress-1> {
	%W toggle current
    }
    $widget bind all <Shift-ButtonPress-1> { 
	blt::HierboxStartSelect %W "set"
    }
    $widget bind all <Control-ButtonPress-1> { 
	blt::HierboxStartSelect %W "toggle"
    }
    $widget bind all <B1-Motion> { 
	if { $bltHierbox(action) != "" } {
	    set bltHierbox(select) on
	}
	set bltHierbox(x) %x
	set bltHierbox(y) %y
	blt::HierboxMotion %W %x %y
    }
    $widget bind all <ButtonRelease-1> { 
	if { $bltHierbox(action) != "" } {
	    blt::HierboxEndSelect %W %x %y
	}
	after cancel $bltHierbox(afterId)
    }
    #
    # Button bindings
    #
    $widget button bind all <ButtonRelease-1> {
	%W toggle current
    }
    $widget button bind all <Enter> {
	%W button activate current
    }
    $widget button bind all <Leave> {
	%W button activate ""
    }
}

# ----------------------------------------------------------------------
# USAGE: blt::HierboxAutoScan <hierbox>
#
# Invoked when the user is selecting elements in a hierbox widget
# and drags the mouse pointer outside of the widget.  Scrolls the
# view in the direction of the pointer.
#
# Arguments:	hierbox		hierarchy widget
#
# ----------------------------------------------------------------------
proc blt::HierboxAutoScan { widget } {
    global bltHierbox
    if { ![winfo exists $widget] } {
	return
    }
#     if { ![winfo exists $widget] ||
#          ![info exists bltHierbox(x)] ||
#          ![info exists bltHierbox(y)] } {
# 	return
#     }
    set x $bltHierbox(x)
    set y $bltHierbox(y)
    if { $y >= [winfo height $widget] } {
	$widget yview scroll 1 units
    } elseif { $y < 0 } {
	$widget yview scroll -1 units
    } 
    blt::HierboxMotion $widget $x $y
    set bltHierbox(afterId) [after 10 blt::HierboxAutoScan $widget]
}

# ----------------------------------------------------------------------
# USAGE blt::HierboxSelect <hierbox> 
#
# Invoked when the user clicks on a <hierbox> widget. The currently
# picked entry is available using the "current" index. The selection
# is cleared and the element at that coordinate is selected.  
#
# Arguments:	hierbox		hierarchy widget
#
# ----------------------------------------------------------------------
proc blt::HierboxSelect { widget } {
    global bltHierbox

    set bltHierbox(action) ""

    # Clear all selections before setting the new anchor.
    $widget selection clear 0 end
    $widget selection set current
    $widget selection anchor current

    $widget see current
    $widget focus current
}

# ----------------------------------------------------------------------
# USAGE: blt::HierboxStartSelect <hierbox> <action>
#
# Invoked when the user clicks on a <hierbox> widget at the screen
# coordinate <x>,<y>.  Marks the start of a "drag" operation for
# selecting multiple elements.  See related HierboxMotion and
# HierboxEndSelect procedures.
#
# Arguments:	hierbox		hierarchy widget
#		action		"set", "clear", "toggle", or "".  
#				Indicates how to manage the selection.
#
# ----------------------------------------------------------------------
proc blt::HierboxStartSelect { widget action } {
    global bltHierbox

    set bltHierbox(action) $action
    switch -- [$widget cget -selectmode] {
	single - active {
	    blt::HierboxSelect $widget
	}
	multiple {
	    $widget selection anchor current
	}
    }
}

# ----------------------------------------------------------------------
# USAGE:	blt::HierboxMotion 
#
# Invoked when the user has clicked on a <hierbox> widget and has
# dragged the mouse to the coordinate <x>,<y>.  Updates a preview
# of the elements being selected.  See related HierboxStartSelect
# and HierboxEndSelect procedures.
#
# Arguments:	hierbox		hierarchy widget
#		x		X-coordinate of mouse pointer
#		y		Y-coordinate of mouse pointer
#
# ----------------------------------------------------------------------
proc blt::HierboxMotion { widget x y } {
    global bltHierbox

    if { $bltHierbox(action) == "" } {
	return
    }
    if {[$widget cget -selectmode] == "multiple" } {
	set node [$widget nearest $x $y]
	$widget selection dragto $node $bltHierbox(action)
    }
}

# ----------------------------------------------------------------------
# USAGE: blt::HierboxEndSelect <hierbox> <x> <y>
#
# Invoked when the user has clicked on a <hierbox> widget and has
# then released the mouse at coordinate <x>,<y>.  Finalizes the
# current selection.  See related HierboxStartSelect and HierboxMotion
# procedures.
#
# Arguments:	hierbox		hierarchy widget
#		x		X-coordinate of mouse pointer
#		y		Y-coordinate of mouse pointer
#
# ----------------------------------------------------------------------
proc blt::HierboxEndSelect { widget x y } {
    global bltHierbox

    if { $bltHierbox(action) != "" } {
	after cancel $bltHierbox(afterId)
    }
    if {[$widget cget -selectmode] == "multiple" } {
	set node [$widget nearest $x $y]
        $widget selection $bltHierbox(action) anchor $node
        $widget selection anchor $node
    }
    set bltHierbox(select) "off"
}

# ----------------------------------------------------------------------
# USAGE: blt::HierboxToggle <hierbox> <index>
# Arguments:	hierbox		hierarchy widget
#
# Invoked when the user presses the space bar.  Toggles the selection
# for the entry at <index>.
# ----------------------------------------------------------------------
proc blt::HierboxToggle { widget index } {
    switch -- [$widget cget -selectmode] {
        single - active {
            set i [$widget index $index]
            if {[lsearch [$widget curselection] $i] < 0} {
                $widget selection clear 0 end
            }
            $widget selection toggle $index
        }
        multiple {
            $widget selection toggle $index
        }
    }
}

# ----------------------------------------------------------------------
# USAGE: blt::HierboxTraverse <hierbox> <where>
#
# Invoked by KeyPress bindings.  Moves the active selection to the
# entry <where>, which is an index such as "up", "down", "prevsibling",
# "nextsibling", etc.
# ----------------------------------------------------------------------
proc blt::HierboxTraverse { widget where } {
    catch {$widget focus $where}
    if {[$widget cget -selectmode] == "active"} {
        $widget selection clear 0 end
        $widget selection set focus
    }
    $widget see focus
}

# ----------------------------------------------------------------------
# USAGE: blt::HierboxPage <hierbox> <where>
# Arguments:	hierbox		hierarchy widget
#
# Invoked by KeyPress bindings.  Pages the current view up or down.
# The <where> argument should be either "top" or "bottom".
# ----------------------------------------------------------------------
proc blt::HierboxPage { widget where } {
    if { [$widget index focus] == [$widget index view.$where] } {
        if {$where == "top"} {
	    $widget yview scroll -1 pages
	    $widget yview scroll 1 units
        } else {
	    $widget yview scroll 1 pages
	    $widget yview scroll -1 units
        }
    }
    update
    $widget activate view.$where
    if {[$widget cget -selectmode] == "active"} {
        $widget selection clear 0 end
        $widget selection set focus
    }
}

# ----------------------------------------------------------------------
# USAGE: blt::HierboxSearch <hierbox> <char>
# Arguments:	hierbox		hierarchy widget
#
# Invoked by KeyPress bindings.  Searches for an entry that starts
# with the letter <char> and makes that entry active.
# ----------------------------------------------------------------------
proc blt::HierboxSearch { widget key } {
    if {[string match {[ -~]} $key]} {
	set last [$widget index focus]
	set next [$widget index next]
	while { $next != $last } {
	    set label [$widget entry cget $next -label]
	    if { [string index $label 0] == $key } {
		break
	    }
	    set next [$widget index -at $next next]
	}
	$widget focus $next
        if {[$widget cget -selectmode] == "active"} {
            $widget selection clear 0 end
            $widget selection set focus
        }
	$widget see focus
    }
}


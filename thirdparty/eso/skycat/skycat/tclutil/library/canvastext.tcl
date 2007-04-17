# canvastext.tcl - utility routines for working with canvase text items
#
# Author: Allan Brighton
# "@(#) $Id: canvastext.tcl,v 1.1.1.1 2006/01/12 16:40:56 abrighto Exp $"


proc ct_move_to_char {canvas item index} {
   $canvas icursor $item $index
}


proc ct_cursor_position_in_line {canvas item} {
    set sxx [ct_search_backward_char $canvas $item "\n"]
    set cxx [$canvas index $item insert]
    return ([expr {$cxx - $sxx}])
}


# search forward from the current insertion cursor position

proc ct_search_forward_char {canvas item char} {
    if {![lempty $item]} {
	set text [lindex [$canvas itemconfigure $item -text] 4]
	loop i [$canvas index $item insert] [$canvas index $item end] {
	    if {$char == [string index $text $i]} {
		return $i
	    }
	}
	return [$canvas index $item end]
    }
    return -1
}


# search backward from the current insertion cursor position

proc ct_search_backward_char {canvas item char} {
    if {![lempty $item]} {
	set text  [lindex [$canvas itemconfigure $item -text] 4]
	loop i [expr {[$canvas index $item insert] - 1}] 0 -1 {
	    if {$char == [string index $text $i]} {
		return $i
	    }
	}
    }
    return -1
}


proc ct_move_forward_or_end {canvas item n} {
    set start [$canvas index $item insert]
    set end [expr {$start + $n}]
    set text  [lindex [$canvas itemconfigure $item -text] 4]

    for { set pos $start } {[expr {$pos < $end}]} {incr pos 1} {
	if { [string index $text $pos] != "\n" } {
	    ct_forward_char $canvas $item 1
	} else {
	    return
	}
    }
}


proc ct_insert_char {canvas item char} {
    $canvas insert $item insert $char
}


proc ct_beginning_of_text {canvas item} {
    $canvas icursor $item 0
}


proc ct_end_of_text {canvas item} {
    $canvas icursor $item end
}


proc ct_forward_char {canvas item n} {
    set position [$canvas index $item insert]
    $canvas icursor $item [expr {$position + $n}]
}


proc ct_backward_char {canvas item n} {
    set position [$canvas index $item insert]
    $canvas icursor $item [expr {$position - $n}]
}


proc ct_delete_char {canvas item} {
    set position [$canvas index $item insert]
    $canvas dchar $item [expr {$position}]
}


proc ct_backward_delete_char {canvas item} {
    set position [$canvas index $item insert]
    $canvas dchars $item [expr {$position - 1}]
}


proc ct_kill_line {canvas item} {
    set text [lindex [$canvas itemconfigure $item -text] 4]
    set ins [$canvas index $item insert]
    if {[string index $text $ins] == "\n"} {
	$canvas dchars $item $ins
    } else {
	$canvas dchars $item $ins [expr {[ct_search_forward_char $canvas $item "\n"] - 1}]
    }
}


proc ct_beginning_of_line {canvas item} {
    set nindex [ ct_search_backward_char $canvas $item "\n" ]
    ct_move_to_char $canvas $item [expr {[expr $nindex] + 1}]
}


proc ct_end_of_line {canvas item} {
    set nindex [ ct_search_forward_char $canvas $item "\n" ]      
    ct_move_to_char $canvas $item [expr {$nindex}]
}

proc ct_next_line {canvas item} {
    set offset [ct_cursor_position_in_line $item $canvas]
    ct_end_of_line
    ct_forward_char $canvas $item 1
    ct_move_forward_or_end $canvas $item [expr {$offset -1}]
}


proc ct_previous_line {canvas item} {
    set offset [expr {[ct_cursor_position_in_line $canvas $item] - 1}]
    ct_beginning_of_line $canvas $item
    ct_backward_char $canvas $item 1
    ct_beginning_of_line $canvas $item
    ct_move_forward_or_end $canvas $item $offset
}


# add the bindings for editing in the text

proc ct_add_bindings {canvas item} {
    $canvas bind $item <KeyPress>  [list ct_insert_char $canvas $item %A]
    $canvas bind $item <Control-d> [list ct_delete_char $canvas $item]
    $canvas bind $item <Delete>    [list ct_backward_delete_char $canvas $item]
    $canvas bind $item <BackSpace> [list ct_backward_delete_char $canvas $item]
    $canvas bind $item <Control-k> [list ct_kill_line $canvas $item]
    # $canvas bind $item <Control-n> [list ct_next_line $canvas $item]
    $canvas bind $item <Control-n> { }
    # $canvas bind $item <Control-p> [list ct_previous_line $canvas $item]
    $canvas bind $item <Control-p> { }
    $canvas bind $item <Control-a> [list ct_beginning_of_line $canvas $item]
    $canvas bind $item <Control-e> [list ct_end_of_line $canvas $item]
    $canvas bind $item <Control-f> [list ct_forward_char $canvas $item 1]
    $canvas bind $item <Control-b> [list ct_backward_char $canvas $item 1]
    $canvas bind $item <Meta-a>    [list ct_beginning_of_text $canvas $item]
    $canvas bind $item <Meta-e>    [list ct_end_of_text $canvas $item]
    #$canvas bind $item <Return>    [list ct_insert_char $canvas $item "\n"]
    $canvas bind $item <Return>    { }
    $canvas bind $item <Control-j> { }
    $canvas bind $item <ButtonRelease-1> "focus $canvas; [list $canvas focus $item]; [list $canvas icursor $item @%x,%y]"
}

# list.tcl - list utility routines for tcl
#
# Copyright (C) 1994 Allan Brighton (abrighto@eso.org)
# "@(#) $Id: list.tcl,v 1.1 1997/11/28 01:38:00 abrighto Exp $"



# Append the given element to the list if it is not
# already there

proc lunion {plist item} {
    upvar $plist list
    foreach i $list {
        if {"$i" == "$item"} {
	    return
        }
    }
    lappend list $item
}


# Remove any elements matching the given string from the list
# and return the result

proc lremove_item {list item} {
    set l {}
    foreach i $list {
        if {"$i" != "$item"} {
            lappend l $i
        }
    }
    return $l
}


# Remove the given element from the list and return the new list

proc lremove {list i} {
    return "[lrange $list 0 [expr $i-1]] [lrange $list [expr $i+1] end]"
}


# return the list in reverse order

proc lreverse {list} {
    set l {}
    set n [llength $list]
    while {$n > 0} {
	lappend l [lindex $list [incr n -1]]
    }
    return $l
}

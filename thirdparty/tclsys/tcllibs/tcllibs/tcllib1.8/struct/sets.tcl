#----------------------------------------------------------------------
#
# sets.tcl --
#
#	Definitions for the processing of sets.
#
# Copyright (c) 2004 by Andreas Kupries.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$
#
#----------------------------------------------------------------------

package require Tcl 8.0

namespace eval ::struct { namespace eval set {} }

namespace eval ::struct::set {
    namespace export set
}

##########################
# Public functions

# ::struct::set::set --
#
#	Command that access all set commands.
#
# Arguments:
#	cmd	Name of the subcommand to dispatch to.
#	args	Arguments for the subcommand.
#
# Results:
#	Whatever the result of the subcommand is.

proc ::struct::set::set {cmd args} {
    # Do minimal args checks here
    if { [llength [info level 0]] == 1 } {
	return -code error "wrong # args: should be \"$cmd ?arg arg ...?\""
    }
    ::set sub S$cmd
    if { [llength [info commands ::struct::set::$sub]] == 0 } {
	::set optlist [info commands ::struct::set::S*]
	::set xlist {}
	foreach p $optlist {
	    lappend xlist [string range $p 16 end]
	}
	return -code error \
		"bad option \"$cmd\": must be [linsert [join [lsort $xlist] ", "] "end-1" "or"]"
    }
    return [uplevel 1 [linsert $args 0 ::struct::set::$sub]]
}

##########################
# Implementations of the functionality.
#

# ::struct::set::Sempty --
#
#       Determines emptiness of the set
#
# Parameters:
#       set	-- The set to check for emptiness.
#
# Results:
#       A boolean value. True indicates that the set is empty.
#
# Side effects:
#       None.
#
# Notes:

proc ::struct::set::Sempty {set} {
    return [expr {[llength $set] == 0}]
}

# ::struct::set::Ssize --
#
#	Computes the cardinality of the set.
#
# Parameters:
#	set	-- The set to inspect.
#
# Results:
#       An integer greater than or equal to zero.
#
# Side effects:
#       None.

proc ::struct::set::Ssize {set} {
    return [llength [Cleanup $set]]
}

# ::struct::set::Scontains --
#
#	Determines if the item is in the set.
#
# Parameters:
#	set	-- The set to inspect.
#	item	-- The element to look for.
#
# Results:
#	A boolean value. True indicates that the element is present.
#
# Side effects:
#       None.

proc ::struct::set::Scontains {set item} {
    return [expr {[lsearch -exact $set $item] >= 0}]
}

# ::struct::set::Sunion --
#
#	Computes the union of the arguments.
#
# Parameters:
#	args	-- List of sets to unify.
#
# Results:
#	The union of the arguments.
#
# Side effects:
#       None.

proc ::struct::set::Sunion {args} {
    switch -exact -- [llength $args] {
	0 {return {}}
	1 {return [lindex $args 0]}
    }
    foreach setX $args {
	foreach x $setX {::set ($x) {}}
    }
    return [array names {}]
}


# ::struct::set::Sintersect --
#
#	Computes the intersection of the arguments.
#
# Parameters:
#	args	-- List of sets to intersect.
#
# Results:
#	The intersection of the arguments
#
# Side effects:
#       None.

proc ::struct::set::Sintersect {args} {
    switch -exact -- [llength $args] {
	0 {return {}}
	1 {return [lindex $args 0]}
    }
    ::set res [lindex $args 0]
    foreach set [lrange $args 1 end] {
	if {[llength $res] && [llength $set]} {
	    ::set res [Intersect $res $set]
	} else {
	    # Squash 'res'. Otherwise we get the wrong result if res
	    # is not empty, but 'set' is.
	    ::set res {}
	    break
	}
    }
    return $res
}

proc ::struct::set::Intersect {A B} {
    if {[llength $A] == 0} {return {}}
    if {[llength $B] == 0} {return {}}

    # This is slower than local vars, but more robust
    if {[llength $B] > [llength $A]} {
	::set res $A
	::set A $B
	::set B $res
    }
    ::set res {}
    foreach x $A {::set ($x) {}}
    foreach x $B {
	if {[info exists ($x)]} {
	    lappend res $x
	}
    }
    return $res
}

# ::struct::set::Sdifference --
#
#	Compute difference of two sets.
#
# Parameters:
#	A, B	-- Sets to compute the difference for.
#
# Results:
#	A - B
#
# Side effects:
#       None.

proc ::struct::set::Sdifference {A B} {
    if {[llength $A] == 0} {return {}}
    if {[llength $B] == 0} {return $A}

    array set tmp {}
    foreach x $A {::set tmp($x) .}
    foreach x $B {catch {unset tmp($x)}}
    return [array names tmp]
}

if {0} {
    # Tcllib SF Bug 1002143. We cannot use the implementation below.
    # It will treat set elements containing '(' and ')' as array
    # elements, and this screws up the storage of elements as the name
    # of local vars something fierce. No way around this. Disabling
    # this code and always using the other implementation (s.a.) is
    # the only possible fix.

    if {[package vcompare [package provide Tcl] 8.4] < 0} {
	# Tcl 8.[23]. Use explicit array to perform the operation.
    } else {
	# Tcl 8.4+, has 'unset -nocomplain'

	proc ::struct::set::Sdifference {A B} {
	    if {[llength $A] == 0} {return {}}
	    if {[llength $B] == 0} {return $A}

	    # Get the variable B out of the way, avoid collisions
	    # prepare for "pure list optimization"
	    ::set ::struct::set::tmp [lreplace $B -1 -1 unset -nocomplain]
	    unset B

	    # unset A early: no local variables left
	    foreach [lindex [list $A [unset A]] 0] {.} {break}

	    eval $::struct::set::tmp
	    return [info locals]
	}
    }
}

# ::struct::set::Ssymdiff --
#
#	Compute symmetric difference of two sets.
#
# Parameters:
#	A, B	-- The sets to compute the s.difference for.
#
# Results:
#	The symmetric difference of the two input sets.
#
# Side effects:
#       None.

proc ::struct::set::Ssymdiff {A B} {
    # symdiff == (A-B) + (B-A) == (A+B)-(A*B)
    if {[llength $A] == 0} {return $B}
    if {[llength $B] == 0} {return $A}
    return [Sunion [Sdifference $A $B] [Sdifference $B $A]]
}

# ::struct::set::Sintersect3 --
#
#	Return intersection and differences for two sets.
#
# Parameters:
#	A, B	-- The sets to inspect.
#
# Results:
#	List containing A*B, A-B, and B-A
#
# Side effects:
#       None.

proc ::struct::set::Sintersect3 {A B} {
    return [list [Sintersect $A $B] [Sdifference $A $B] [Sdifference $B $A]]
}

# ::struct::set::Sequal --
#
#	Compares two sets for equality.
#
# Parameters:
#	a	First set to compare.
#	b	Second set to compare.
#
# Results:
#	A boolean. True if the lists are equal.
#
# Side effects:
#       None.

proc ::struct::set::Sequal {A B} {
    ::set A [Cleanup $A]
    ::set B [Cleanup $B]

    # Equal if of same cardinality and difference is empty.

    if {[::llength $A] != [::llength $B]} {return 0}
    return [expr {[llength [Sdifference $A $B]] == 0}]
}


proc ::struct::set::Cleanup {A} {
    # unset A to avoid collisions
    if {[llength $A] < 2} {return $A}
    foreach [lindex [list $A [unset A]] 0] {.} {break}
    return [info locals]
}

# ::struct::set::Sinclude --
#
#	Add an element to a set.
#
# Parameters:
#	Avar	-- Reference to the set variable to extend.
#	element	-- The item to add to the set.
#
# Results:
#	None.
#
# Side effects:
#       The set in the variable referenced by Avar is extended
#	by the element (if the element was not already present).

proc ::struct::set::Sinclude {Avar element} {
    # Avar = Avar + {element}
    upvar 1 $Avar A
    if {![Scontains $A $element]} {
	lappend A $element
    }
    return
}

# ::struct::set::Sexclude --
#
#	Remove an element from a set.
#
# Parameters:
#	Avar	-- Reference to the set variable to shrink.
#	element	-- The item to remove from the set.
#
# Results:
#	None.
#
# Side effects:
#       The set in the variable referenced by Avar is shrunk,
#	the element remove (if the element was actually present).

proc ::struct::set::Sexclude {Avar element} {
    # Avar = Avar + {element}
    upvar 1 $Avar A
    while {[::set pos [lsearch -exact $A $element]] >= 0} {
	::set A [lreplace [K $A [::set A {}]] $pos $pos]
    }
    return
}

# ::struct::set::Sadd --
#
#	Add a set to a set. Similar to 'union', but the first argument
#	is a variable.
#
# Parameters:
#	Avar	-- Reference to the set variable to extend.
#	B	-- The set to add to the set in Avar.
#
# Results:
#	None.
#
# Side effects:
#       The set in the variable referenced by Avar is extended
#	by all the elements in B.

proc ::struct::set::Sadd {Avar B} {
    # Avar = Avar + B
    upvar 1 $Avar A
    ::set A [Sunion [K $A [::set A {}]] $B]
    return
}

# ::struct::set::Ssubtract --
#
#	Remove a set from a set. Similar to 'difference', but the first argument
#	is a variable.
#
# Parameters:
#	Avar	-- Reference to the set variable to shrink.
#	B	-- The set to remove from the set in Avar.
#
# Results:
#	None.
#
# Side effects:
#       The set in the variable referenced by Avar is shrunk,
#	all elements of B are removed.

proc ::struct::set::Ssubtract {Avar B} {
    # Avar = Avar - B
    upvar 1 $Avar A
    ::set A [Sdifference [K $A [::set A {}]] $B]
    return
}

# ::struct::set::Ssubsetof --
#
#	A predicate checking if the first set is a subset
#	or equal to the second set.
#
# Parameters:
#	A	-- The possible subset.
#	B	-- The set to compare to.
#
# Results:
#	A boolean value, true if A is subset of or equal to B
#
# Side effects:
#       None.

proc ::struct::set::Ssubsetof {A B} {
    # A subset|== B <=> (A == A*B)
    return [Sequal $A [Sintersect $A $B]]
}

# ::struct::set::K --
# Performance helper command.

proc ::struct::set::K {x y} {::set x}

# ### ### ### ######### ######### #########
## Ready

namespace eval ::struct {
    # Get 'set::set' into the general structure namespace.
    namespace import -force set::set
    namespace export set
}
package provide struct::set 2.1

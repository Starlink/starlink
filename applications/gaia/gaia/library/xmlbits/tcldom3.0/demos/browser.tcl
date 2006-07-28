#!/bin/sh
# \
exec wish "$0" "$@"

# browser.tcl --
#
#	A mini XML browser
#
# Copyright (c) 2003 Zveno Pty Ltd
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id$

package require dom
package require domtree
package require domtext

proc Open {} {
    global tree text doc view

    set fname [tk_getOpenFile]
    if {![string length $fname]} {
        return {}
    }

    set ch [open $fname]
    set newdoc [dom::parse [read $ch]]
    close $ch

    $tree configure -rootnode {}
    $text configure -rootnode {}
    catch {dom::destroy $doc}

    set doc $newdoc
    if {[lsearch $view tree] >= 0} {
	$tree configure -rootnode $doc
    }
    if {[lsearch $view text] >= 0} {
	$text configure -rootnode $doc
    }

    return {}
}

proc View:tree {} {
    global view

    set view tree

    .controls.view configure -text {Tree & Text} -command View:both

    return {}
}
proc View:both {} {
    global view

    set view {tree text}

    .controls.view configure -text {Tree Only} -command View:tree

    return {}
}

frame .controls
button .controls.open -text Open -command Open
button .controls.view
View:both
grid .controls.open .controls.view -sticky w

set p [panedwindow .panes -orient horizontal]

grid .controls -row 0 -column 0 -sticky ew
grid $p -row 1 -column 0 -sticky news
grid rowconfigure . 1 -weight 1
grid columnconfigure . 0 -weight 1

labelframe $p.tree -text {Tree View}
$p add $p.tree -minsize 200
set tree [domtree::create $p.tree.t \
	      -yscrollcommand [list $p.tree.y set] \
	      -xscrollcommand [list $p.tree.x set]]
scrollbar $p.tree.y -orient vertical -command [list $p.tree.t yview]
scrollbar $p.tree.x -orient horizontal -command [list $p.tree.t xview]
grid $p.tree.t -row 0 -column 0 -sticky news
grid $p.tree.y -row 0 -column 1 -sticky ns
grid $p.tree.x -row 1 -column 0 -sticky ew
grid rowconfigure $p.tree 0 -weight 1
grid columnconfigure $p.tree 0 -weight 1

labelframe $p.text -text {Source View}
$p add $p.text -minsize 200
set text [domtext::create $p.text.t \
	      -yscrollcommand [list $p.text.y set] \
	      -xscrollcommand [list $p.text.x set]]
scrollbar $p.text.y -orient vertical -command [list $p.text.t yview]
scrollbar $p.text.x -orient horizontal -command [list $p.text.t xview]
grid $p.text.t -row 0 -column 0 -sticky news
grid $p.text.y -row 0 -column 1 -sticky ns
grid $p.text.x -row 1 -column 0 -sticky ew
grid rowconfigure $p.text 0 -weight 1
grid columnconfigure $p.text 0 -weight 1


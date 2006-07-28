# domtree-treectrl.tcl --
#
#	A megawidget to support display of a DOM hierarchy
#	based on the treectrl widget.
#
#	This widget both generates and reacts to DOM Events.
#
# This package features ordered and non-unique directories and items.
# Paths are managed as live links into a DOM hierarchy.
#
# Copyright (c) 2005 Explain
# http://www.explain.com.au/
# Copyright (c) 2004 Zveno Pty Ltd
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id$

package provide domtree::treectrl 3.1

# We need the treectrl widget

package require treectrl 1.1

# We need the DOM
# V2.0 gives us Level 2 event model
# V2.1 gives us libxml2

package require dom 3.1

namespace eval domtree {
    # Just make sure this namespace exists
    variable exists {}
}
namespace eval domtree::treectrl {
    variable defaults
    array set defaults {
	showlength 20
	showtextcontent 0
	showelementid 0
    }

    catch {font create [namespace current]::bold -weight bold}

    proc ::domtree::treectrl { path args } { return [eval domtree::treectrl::create $path $args] }

    # We may be able to use tktreectrl's event mechanism
    # to exactly match treectrl events to DOM events
    variable eventTypeMap
    array set eventTypeMap {
	ButtonPress	mousedown
	ButtonRelease	mouseup
	Enter		mouseover
	Leave		mouseout
	Motion		mousemove
	FocusIn		DOMFocusIn
	FocusOut	DOMFocusOut
    }
}

# domtree::treectrl::create --
#
#	Create a DOM Treectrl widget
#
# Arguments:
#	path	widget path
#	args	configuration options
#
# Results:
#	Tree widget created

proc domtree::treectrl::create {path args} {
    upvar \#0 [namespace current]::Widget$path widget

    eval frame $path -bd 0 -relief flat -takefocus 0 \
	    -class domtree::treectrl -highlightthickness 0

    bindtags $path [list $path domtree::treectrl [winfo toplevel $path] all]

    set tree [eval treectrl $path.tree -showroot yes -showrootbutton yes \
		  -showbuttons yes -showlines yes \
		  -itemheight 0 \
		  -openbuttonimage ::domtree::collapse -closedbuttonimage ::domtree::expand]

    $path.tree column create -expand yes -text Elements -tag element
    $path.tree column create -text Attributes -tag attr
    $path.tree column create -text Depth -tag depth

    $path.tree element create e1 image -image {::domtree::element {open} ::domtree::element {}}
    $path.tree element create Edocument image -image ::domtree::textNode
    $path.tree element create EtextNode text
    $path.tree element create Ecomment image -image ::domtree::Comment
    $path.tree element create e3 text \
	-fill [list [$path cget -highlightcolor] {selected focus}] \
	-font [list [namespace current]::bold {}]
    $path.tree element create e4 text -fill blue
    $path.tree element create e6 text
    $path.tree element create e5 rect -showfocus yes \
	-fill [list [$path cget -highlightbackground] {selected focus} gray {selected !focus}]

    $path.tree style create Selement
    $path.tree style elements Selement {e5 e1 e3 e4}
    $path.tree style layout Selement e1 -padx {0 4} -expand ns
    $path.tree style layout Selement e3 -padx {0 4} -expand ns
    $path.tree style layout Selement e4 -padx {0 6} -expand ns
    $path.tree style layout Selement e5 -union [list e3] -iexpand ns -ipadx 2

    $path.tree style create Sdocument
    $path.tree style elements Sdocument {e5 Edocument e3 e4}
    $path.tree style layout Sdocument Edocument -padx {0 4} -expand ns
    $path.tree style layout Sdocument e3 -padx {0 4} -expand ns
    $path.tree style layout Sdocument e4 -padx {0 6} -expand ns
    $path.tree style layout Sdocument e5 -union [list e3] -iexpand ns -ipadx 2

    $path.tree style create StextNode
    $path.tree style elements StextNode EtextNode
    $path.tree style layout StextNode EtextNode -padx {0 4} -squeeze x

    $path.tree style create Scomment
    $path.tree style elements Scomment {e5 Ecomment e3 e4}
    $path.tree style layout Scomment Ecomment -padx {0 4} -expand ns
    $path.tree style layout Scomment e3 -padx {0 4} -expand ns
    $path.tree style layout Scomment e4 -padx {0 6} -expand ns
    $path.tree style layout Scomment e5 -union [list e3] -iexpand ns -ipadx 2

    $path.tree style create s3
    $path.tree style elements s3 {e6}
    $path.tree style layout s3 e6 -padx 6 -expand ns

    # Create custom event to allow mapping to DOM nodes
    $path.tree notify install event MapDOMNode

    # Set various bindings to generate DOM events

    if {0} {
    foreach event {ButtonRelease ButtonPress Enter Leave Motion} {
	$path.tree bindImage <$event> [namespace code [list _node_mouse_event $event {} $path]]
	$path.tree bindText <$event> [namespace code [list _node_mouse_event $event {} $path]]
	foreach modifier {Control Shift Alt Meta Double} {
	    $path.tree bindImage <$modifier-$event> [namespace code [list _node_mouse_event $event $modifier $path]]
	    $path.tree bindText <$modifier-$event> [namespace code [list _node_mouse_event $event $modifier $path]]
	}
    }
    }

    grid $tree -row 0 -column 0 -sticky news
    grid rowconfigure $path 0 -weight 1
    grid columnconfigure $path 0 -weight 1

    rename $path ::$path:cmd
    proc ::$path { cmd args } "return \[eval domtree::treectrl::cmd $path \$cmd \$args\]"

    array set widget {
	-rootnode {}
	-populate normal
    }

    foreach {option value} $args {
	configure $path $option $value
    }

    return $path
}

# domtree::treectrl::see --
#
#	Display a DOM node in the tree.
#
# Arguments:
#	path	widget path
#	dnode	DOM node
#
# Results:
#	The tree node for the corresponding DOM node is expanded,
#	all parent nodes are also expanded.
#	Returns the id of the tree item.

proc domtree::treectrl::see {path dnode} {
    foreach pathnode [dom::node path $dnode] {
	$path.tree expand [_dnode_to_treeid $path $pathnode]
	update idletasks
    }
    set id [_dnode_to_treeid $path $dnode]
    $path.tree see $id
    return $id
}

# domtree::treectrl::cmd --
#
#	Widget command
#
# Arguments:
#	path	widget path
#	method	command method
#	args	method arguments
#
# Results:
#	Depends on method.

proc domtree::treectrl::cmd {path method args} {
    return [eval [list $method $path] $args]
}

# domtree::treectrl::cget --
#
#	Implements the cget method
#
# Arguments:
#	path	widget path
#	option	configuration option
#
# Results:
#	Returns value of option

proc domtree::treectrl::cget {path option} {
    switch -- $option {
	-rootnode -
	-populate {
	    upvar \#0 [namespace current]::Widget$path widget

	    return $widget($option)
	}
	default {
	    return [$path.tree cget $option]
	}
    }
}

# domtree::treectrl::configure --
#
#	Implements the configure method
#
# Arguments:
#	path	widget path
#	args	configuration options
#
# Results:
#	Sets value of options

proc domtree::treectrl::configure {path args} {
    if {[catch {eval configure:dbg [list $path] $args} msg]} {
	puts stderr "domtree::treectrl::configure incurred error\n$msg"
    }
}

proc domtree::treectrl::configure:dbg {path args} {
    set res {}

    foreach {option value} $args {
	switch -- $option {
	    -rootnode {
		upvar \#0 [namespace current]::Widget$path widget

		if {$widget(-rootnode) != ""} {
		    $path.tree item delete all
		    _dom_unmap $path $widget(-rootnode)
		}

		if {$value != ""} {
		    set widget(-rootnode) $value
		    _add_node $path 0 $value
		}
	    }
	    -populate {
		upvar \#0 [namespace current]::Widget$path widget

		switch -- $value {
		    {} -
		    normal {
			set widget(-populate) normal
		    }
		    lazy {
			set widget(-populate) lazy
		    }
		    default {
			return -code error "unknown value \"$value\" for option \"-populate\""
		    }
		}
	    }
	    default {
		return [$path.tree configure $option $value]
	    }
	}
    }

    # May need to add these to above switch code
    if {0} {
		# Listen for UI events
		dom::node addEventListener $docel DOMActivate [namespace code [list _node_selected $path]] -usecapture 1

		# Listen for mutation events
		dom::node addEventListener $docel DOMSubtreeModified [namespace code [list _node_tree_modified $path]] -usecapture 1
		dom::node addEventListener $docel DOMNodeInserted [namespace code [list _node_inserted $path]] -usecapture 1
		dom::node addEventListener $docel DOMNodeRemoved [namespace code [list _node_removed $path]] -usecapture 1
		dom::node addEventListener $docel DOMCharacterDataModified [namespace code [list _node_data_modified $path]] -usecapture 1
		dom::node addEventListener $docel DOMAttrModified [namespace code [list _node_attr_modified $path]] -usecapture 1
		dom::node addEventListener $docel DOMAttrRemoved [namespace code [list _node_attr_removed $path]] -usecapture 1
    }

    return $res
}

# domtree::treectrl::refresh --
#
#	Updates the Tree display with the value of a node
#
# Arguments:
#	path	widget path
#	node	DOM node
#
# Results:
#	May change node display

proc domtree::treectrl::refresh {path node} {
    _refresh $path $node
    return {}
}

# domtree::treectrl::xview --
#
#	Implement xview method
#
# Arguments:
#	path	widget path
#	args	additional arguments
#
# Results:
#	Depends on Tree xview method

proc domtree::treectrl::xview {path args} {
    eval $path.tree xview $args
}

# domtree::treectrl::yview --
#
#	Implement yview method
#
# Arguments:
#	path	widget path
#	args	additional arguments
#
# Results:
#	Depends on Tree yview method

proc domtree::treectrl::yview {path args} {
    eval $path.tree yview $args
}

# domtree::treectrl::selection --
proc domtree::treectrl::selection {path args} {
    eval $path.tree selection $args
}

# domtree::treectrl::find --
#
#	Find DOM node at given location
#
# Arguments:
#	path	widget path
#	findInfo	location
#	confine
#
# Results:
#	DOM node at location

proc domtree::treectrl::find {path findInfo {confine {}}} {
    set tnode [$path.tree find $findInfo $confine]
    return [_treeid_to_dnode $tnode]
}

# Procedures to implement display

# domtree::treectrl::_refresh --
#
#	Configure node with appropriate images, labels, etc
#
# Arguments:
#	path	widget path
#	node	DOM node
#	args	additional options
#
# Results:
#	Tree node may have image or label changed

proc domtree::treectrl::_refresh {path node args} {

    switch [set nodetype [::dom::node cget $node -nodeType]] {
	document -
	documentFragment -
	element {
	    set label [dom::node cget $node -nodeName]
	    set icon ::domtree::element

	    if {![string compare $nodetype element]} {

		# ID attribute display
		if {[Widget::getoption $path -showelementid]} {
		    array set attributes [array get [::dom::node cget $node -attributes]]
		    if {[catch {
			append label "    (id $attributes(id))"
		    }] && [catch {
			append label "    (ID $attributes(ID))"
		    }]} {}
		}

		if {[Widget::getoption $path -showtextcontent]} {
		    # Text content display
		    set temp [_refresh_text_content_display_find_text $node [Widget::getoption $path -showlength]]
		    if {[string length $temp]} {
			append label "    \[ [_refresh_string_trim $temp [Widget::getoption $path -showlength]] \]"
		    }
		}

	    }

	    if {![string length [dom::node parent $node]]} {
		# Root node is special
		return {}
	    }

	}
	textNode {
	    array set opts [list -label [dom::node cget $node -nodeValue]]
	    array set opts $args
	    set label [_refresh_string_trim [string trim $opts(-label)] [Widget::getoption $path -showlength]]
	    set icon ::domtree::textNode

	    # Also do the ancestors
	    foreach ancestor [lrange [lreplace [::dom::node path $node] end end] 1 end] {
		_refresh $path $ancestor
	    }
	}
	processingInstruction {
	    set label [string trim [dom::node cget $node -nodeName]]
	    set icon ::domtree::PI
	}
	docType {
	    set label {}
	    set icon ::domtree::DocType
	}
	comment {
	    set label [_refresh_string_trim [string trim [::dom::node cget $node -nodeValue]] [Widget::getoption $path -showlength]]
	    set icon ::domtree::Comment
	}
	entityReference {
	    set label [::dom::node cget $node -nodeName]
	    set icon ::domtree::EntityReference
	}
	default {
	    set label $nodetype
	    set icon ::domtree::other
	}
    }

    catch {
	$path.tree itemconfigure [_dom_to_tree $node] -image $icon
	$path.tree itemconfigure [_dom_to_tree $node] -text $label
    }

    return {}
}

# domtree::treectrl::_refresh_text_content_display_find_text --
#
#	Searches given element for text.
#	In future could use XPath - just get the string value
#	of the node.
#
# Arguments:
#	node	DOM element node to search
#	len	amount of text to return
#
# Results:
#	Returns string

proc domtree::treectrl::_refresh_text_content_display_find_text {node len} {
    switch -- $len {
	0 {
	    return {}
	}
	default {
	    set text {}
	    foreach child [::dom::node children $node] {
		switch [::dom::node cget $child -nodeType] {
		    document -
		    documentFragment -
		    element {
			append text \
				[_refresh_text_content_display_find_text $child [expr $len - [string length $text]]]
		    }
		    textNode {
			append text [string range \
				[::dom::node cget $child -nodeValue] \
				0 [expr $len - [string length $text]] \
			]
		    }
		    default {
			# Nothing to do
		    }
		}
		if {[string length $text] >= $len} {
		    return $text
		}
	    }

	    return $text

	}
    }

    return {}
}

# domtree::treectrl::_refresh_all --
#
#	Updates display of all tree nodes
#
# Arguments:
#	path	widget pathname
#	node	Tree node
#
# Results:
#	Returns empty string

proc domtree::treectrl::_refresh_all {path node} {
    foreach child [$path.tree nodes $node] {
	_refresh $path [_tree_to_dom $child]
	_refresh_all $path $child
    }

    return {}
}

# domtree::treectrl::_refresh_string_trim --
#
#	Massage text for display
#
# Arguments:
#	text	text string
#	max	maximum length for string
#
# Results:
#	Returns string

proc domtree::treectrl::_refresh_string_trim {text max} {
    if {[string length $text] > $max} {
	set text [string range $text 0 [expr $max - 3]]...
    }
    if {[info tclversion] >= 8.1} {
	set dot \u2022
    } else {
	set dot { }
    }
    regsub -all [format {[%s%s%s%s]+} \n \r { } \t] $text $dot text
    return $text
}

# domtree::treectrl::_node_selected --
#
#	A node has been selected.
#
#	This is invoked via a DOM event.
#
# Arguments:
#	path	widget path
#	evid	event node

proc domtree::treectrl::_node_selected {path evid} {

    set domnode [dom::event cget $evid -target]

    # Temporarily remove the -selectcommand callback
    # to avoid an infinite loop (continually posting DOM click events)
    set cmd [$path.tree cget -selectcommand]
    $path.tree configure -selectcommand {}

    $path.tree selection set [_dom_to_tree $domnode]

    $path.tree configure -selectcommand $cmd

    return {}
}

# domtree::treectrl::_select_node --
#
#	A tree node has been selected.
#
# Arguments:
#	path	widget path
#	tree	tree path
#	tnode	tree node

proc domtree::treectrl::_select_node {path tree tnode} {

    dom::event postMouseEvent [_tree_to_dom $tnode] click -detail 1

    return {}
}

# domtree::treectrl::_node_mouse_event --
#
#	Generate DOM Mouse Event
#
# Arguments:
#	event	event type
#	mod	modifier
#	path	widget path
#	tnode	tree node
#
# Results:
#	Event synthesized for DOM

proc domtree::treectrl::_node_mouse_event {event mod path tnode} {
    variable eventTypeMap

    set type $event
    catch {set type $eventTypeMap($event)}

    set evid [dom::document createEvent [_tree_to_dom $tnode] $type]
    dom::event initMouseEvent $evid $type 1 1 {} 0 0 0 0 0 \
	    [expr {$mod == "Control"}] \
	    [expr {$mod == "Alt"}] \
	    [expr {$mod == "Shift"}] \
	    [expr {$mod == "Meta"}] \
	    0 {}
    dom::node dispatchEvent [_tree_to_dom $tnode] $evid
    dom::destroy $evid

    # ButtonRelease events also generate DOMActivate events

    if {![string compare $event "ButtonRelease"]} {
	set detail 1
	if {![string compare $mod "Double"]} {
	    set detail 2
	}
	dom::event postUIEvent [_tree_to_dom $tnode] DOMActivate -detail $detail
    }

    return {}
}

# domtree::treectrl::_node_ui_event --
#
#	Generate DOM UI Event
#
# Arguments:
#	event	event type
#	path	widget path
#	tnode	tree node
#
# Results:
#	Event synthesized for DOM

proc domtree::treectrl::_node_ui_event {event path tnode} {
    variable eventTypeMap

    set type $event
    catch {set type $eventTypeMap($event)}
    dom::event postUIEvent [_tree_to_dom $tnode] $type

    return {}
}

# domtree::treectrl::_add_node --
#
#	Recurse DOM structure, inserting tree nodes as we go.
#
#
# Arguments:
#	w	tree widget path
#	tnode	tree node to add children to
#	dnode	DOM node corresponding to tree path above
#
# Results:
#	Nodes added to tree

proc domtree::treectrl::_add_node {path tnode dnode} {
    upvar \#0 [namespace current]::Widget$path widget

    switch [dom::node cget $dnode -nodeType] {
	document {
	    set nodename {}
	    set hasChildren 1
	    set text {}
	    set attrs {}
	}
	element {
	    set nodename [$dnode cget -nodeName]
	    set hasChildren [$dnode hasChildNodes]
	    set text {}
	    set attrs {}
	    foreach atnode [dom::node selectNode $dnode @*] {
		lappend attrs [dom::node cget $atnode -nodeName]
	    }
	}
	textNode {
	    set nodename {}
	    set hasChildren 0
	    set text [$dnode cget -nodeValue]
	    set attrs {}
	}
	default {
	    set nodename [dom::node cget $dnode -nodeType]
	    set hasChildren 0
	    set text {}
	    set attrs {}
	}
    }

    set id [$path.tree item create]
    if {$tnode != ""} {
	$path.tree item lastchild $tnode $id
    }
    $path.tree item configure $id -button $hasChildren
    switch [dom::node cget $dnode -nodeType] {
	textNode {
	    $path.tree item style set $id 0 S[dom::node cget $dnode -nodeType]
	    $path.tree item text $id 0 $text
	}
	default {
	    $path.tree item style set $id 0 S[dom::node cget $dnode -nodeType] \
		1 s3 2 s3
	    $path.tree item complex $id \
		[list [list e3 -text $nodename]] \
		[list [list e6 -text $attrs]] \
		[list [list e6 -text [llength [dom::node path $dnode]]]]
	}
    }

    # Create a two-way mapping between DOM node and tree id
    $path.tree notify bind $id <MapDOMNode> [list [namespace current]::_domid $dnode]
    dom::node addEventListener $dnode DOMActivate [list [namespace current]::_treeid $path $id]

    # Implement lazy population of the tree widget
    if {$widget(-populate) == "lazy"} {
	$path.tree collapse $id
	after idle [list $path.tree notify bind $id <Expand-before> [namespace code [list _node_open $path %I $id $dnode]]]
    }
    if {$widget(-populate) == "normal"} {
	foreach dchild [dom::node children $dnode] {
	    _add_node $path $id $dchild
	}
    }

    return {}
}
# These should not be called
proc domtree::treectrl::_domid {dnode} {
    return $dnode
}
proc domtree::treectrl::_treeid {id} {
    return $id
}

# domtree::treectrl::_dnode_to_treeid --
#
#	Find the tree item for a DOM node
#
# Arguments:
#	path	widget path
#	dnode	DOM node
#
# Results:
#	Returns a tree item descriptor

proc domtree::treectrl::_dnode_to_treeid {path dnode} {
    set listener {}
    foreach l [dom::node addEventListener $dnode DOMActivate] {
	foreach {key dpath value} $l break
	if {[string equal $path $dpath] && \
		[string equal $key "[namespace current]::_treeid"]} {
	    return $value
	}
    }

    return {}
}

# domtree::treectrl::_treeid_to_dnode --
#
#	Find the DOM node for a tree item
#
# Arguments:
#	path	widget path
#	id	item descriptor
#
# Results:
#	Returns a DOM node token

proc domtree::treectrl::_treeid_to_dnode {path id} {
    return [lindex [$path.tree notify bind $id <MapDOMNode>] end]
}

# domtree::treectrl::_dom_unmap --
#
#	Remove all event listeners for a tree widget.
#
# Arguments:
#	path	widget path
#	node	DOM node
#
# Results:
#	Returns empty string.
#	Event listeners may be removed from DOM document nodes.

proc domtree::treectrl::_dom_unmap {path node} {
    # Crashing bug in TclDOM v3.1 prevents us from cleaning up
    return {}

    foreach listener [dom::node addEventListener $node DOMActivate] {
	foreach {key dpath value} $listener break
	if {[string match [namespace current]::_* $key] && \
		[string equal $dpath $path]} {
	    # This is one of ours
	    dom::node removeEventListener $node DOMActivate $listener
	}
    }

    foreach child [dom::node children $node] {
	_dom_unmap $path $child
    }
}

# domtree::_set_client_data --
#
#	Manage data for tree nodes
#
# Arguments:
#	path	widget path
#	node	tree node
#	field	field name
#	value	value for field
#
# Results:
#	Item's configuration changed

proc domtree::_set_client_data {path node field value} {
    array set nodeinfo [$path.tree itemcget $node -data]
    set nodeinfo($field) $value
    $path.tree itemconfigure $node -data [array get nodeinfo]
}

# domtree::_unset_client_data --
#
#	Manage data for tree nodes
#
# Arguments:
#	path	widget path
#	node	tree node
#	field	field name to unset
#
# Results:
#	Item's configuration changed

proc domtree::_unset_client_data {path node field} {
    array set nodeinfo [$path.tree itemcget $node -data]
    catch {unset nodeinfo($field)}
    $path.tree itemconfigure $node -data [array get nodeinfo]
}

# domtree::_node_open --
#
#	Invoked when a tree item is opened and
#	the tree is being populated lazily.
#
# Arguments:
#	path	widget path
#	id	tree item
#	dnode	DOM node
#
# Results:
#	Tree nodes may be added

proc domtree::treectrl::_node_open {path tnode id dnode} {
    if {[string equal $tnode $id]} {
	$path.tree notify bind $id <Expand-before> {}
	foreach dchild [dom::node children $dnode] {
	    _add_node $path $id $dchild
	}
    }

    return {}
}

# domtree::_node_tree_modified --
#
#	Invoked when the node's subtree has changed.
#	Could be because a child node has been removed.
#
#	Refresh the
#	display of the node, since if textual content
#	is enabled the node's string value may have
#	changed.
#
# Arguments:
#	path	widget path
#	evid	DOM event node
#
# Results:
#	Tree nodes inserted or removed

proc domtree::_node_tree_modified {path evid} {

    set target [dom::event cget $evid -target]
    set children [dom::node children $target]
    set branch [Tree::nodes $path.tree [_dom_to_tree $target]]
    if {[llength $children] < [llength $branch]} {
	for {set idx 0} {$idx < [llength $branch]} {incr idx} {
	    if {![string length [lindex $children $idx]] || \
		    [_dom_to_tree [lindex $children $idx]] != [lindex $branch $idx]} {
		$path.tree delete [lindex $branch $idx]
		break
	    }
	}
    }

    _refresh $path [dom::event cget $evid -currentNode]

    return {}
}

# domtree::_node_inserted --
#
#	A node has been inserted.
#
# Arguments:
#	path	widget path
#	evid	DOM event node
#
# Results:
#	Insert tree node

proc domtree::_node_inserted {path evid} {

    # Find where the node was inserted into the child list
    set newnode [dom::event cget $evid -target]
    set parent [dom::node parent $newnode]
    set children [dom::node children $parent]
    set idx [lsearch $children $newnode]

    # Get old tree info
    set tparent [_dom_to_tree $parent]
    set branch [Tree::nodes $path.tree $tparent]

    if {$idx > [llength $branch]} {
	# Append the new node to the branch
	$path.tree insert end $tparent [_dom_to_tree $newnode]
    } else {
	# Insert the new node into the branch
	$path.tree insert $idx $tparent [_dom_to_tree $newnode]
    }

    _refresh $path $newnode
    _add_node $path [_dom_to_tree $newnode] $newnode

    return {}
}

# domtree::_node_removed --
#
#	A node has been removed.
#
# Arguments:
#	path	widget path
#	evid	DOM event node
#
# Results:
#	Remove tree node

proc domtree::_node_removed {path evid} {

    set oldnode [dom::event cget $evid -target]
    Tree::delete $path.tree [_dom_to_tree $oldnode]

    return {}
}

# domtree::_node_data_modified --
#
#	Character data has changed
#
# Arguments:
#	path	widget path
#	evid	DOM L2 event node
#
# Results:
#	Tree display updated

proc domtree::_node_data_modified {path evid} {
    _refresh $path [dom::event cget $evid -target] \
	    -label [dom::event cget $evid -newValue]
    return {}
}

# domtree::_node_attr_modified --
#
#	Attribute value modified
#
# Arguments:
#	path	widget path
#	evid	DOM L2 event node
#
# Results:
#	Display updated

proc domtree::_node_attr_modified {path evid} {
    _refresh $path [dom::event cget $evid -target]
    return {}
}

# domtree::_node_attr_removed --
#
#	Attribute removed
#
# Arguments:
#	path	widget path
#	evid	DOM L2 event node
#
# Results:
#	Display updated

proc domtree::_node_attr_removed {path evid} {
    _refresh $path [dom::event cget $evid -target]
    return {}
}

### Image data

image create photo ::domtree::element -data {R0lGODlhEAAQANX/AP7///3//vv+/vn+/fj+/ff9/fb9/fL8/PL6+e/8++78+u37+uz7+ur7
+ef6+d349tv49df18tT289P288718sPz773y7bPw6qzo4qTt5o3o4Ivn4IHb0nTj2XPj2Wrh
107bzzDVxxnQwRecjBCtmRCtmA+AdA6Hew21oQ2NgQq+rQqjlQpoXApmWgm/rQeekAXMuwXG
tQTMu////wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACH/C0FET0JFOklS
MS4wAt7tACH5BAEAADMALAAAAAAQABAAAAaRwJlwCBgajZCKBeE4GhuXCCIinQycs+iUA8N4
HgHnZyuTcUoZAyAwIAgA5LKMNGIckksHgiOXoRAnElpUCCB8MlKEGolcGBsiMIwyGCFxHBMd
LpZxZQMPnDJ7fSVConIlKocyJSNChqcjKzEwqyMmQo+0rCYpLyUmwC1CmL/BLBQZIy3KQp7J
yy0KAgUJCwcCQQA7
}
image create photo ::domtree::textNode -data {R0lGODlhEAAOAJH/AP///39/fwAAAP///yH/C0FET0JFOklSMS4wAt7tACH5BAEAAAMALAAA
AAAQAA4AAAI0nIUpxi0AIWoOhAveouPFnHDPhV1CQHmfhFYkmbWMup6p9QbxvbJ3rrNVejuH
4ihjAF+GAgA7
}
image create photo ::domtree::PI -data {R0lGODdhEAAOAPEAALLA3AAAAAAA/////ywAAAAAEAAOAAACL4yPoBvi78Jio9oqJwh3oG90
DfSEF9dhKIioGFmqR4phFL3eaa6g+6ETaTYsw6IAADs=
}
image create photo ::domtree::DocType -data {R0lGODlhEAAQAKL/APfQ0MmZmYJfX2YAAEoBAf///wAAAAAAACH/C0FET0JFOklSMS4wAt7t
ACH5BAEAAAUALAAAAAAQABAAAAM7WDKyUjBGB8AaUl4RQFhZNIxM8D2hQJBNgIUKOZ5wsbJu
fcmNfrM1iEoWnIyKqRGqWHoFd0sdEOmAJAAAOw==
}
image create photo ::domtree::Comment -data {R0lGODlhEAAQAKL/AP///8fHx7CwsJ6enpycnHp6egAAAP///yH/C0FET0JFOklSMS4wAt7t
ACH5BAEAAAcALAAAAAAQABAAAANDeLrcazBGZ4C917CKTegPBnigwmVDJ5iikWbEelpuV8hi
bhTEMY8vGo+VE8Yeswhv1eDsCkuHb8Qj9KSRo9RniDG3CQA7
}
image create photo ::domtree::EntityReference -data {R0lGODlhEAAQALP/AP7+/vfQ0NOsrMmZmci5uYMwMIJfX2YAAEoBAf///wAAAAAAAAAAAAAA
AAAAAAAAACH/C0FET0JFOklSMS4wAt7tACH5BAEAAAkALAAAAAAQABAAAARPMEl5jAlhzJ2O
r1WmbV+CfEdGVtzJTp4xwq90XuvBmZVAeTtbxVAK0nQTYg11mKUGyJJL8ykQOiwAr1nsyDau
mgn3+8xsFwuzeUYopR5NBAA7
}
image create photo ::domtree::other -data {R0lGODlhEAAOAKL/AP///39/fxAQEAAAAP///wAAAAAAAAAAACH/C0FET0JFOklSMS4wAt7t
ACH5BAEAAAQALAAAAAAQAA4AAAM4SDSj/m8E0ByrdtI1wI4aFV6ZR5kiJpmrJ6kj+pbuGMCs
fIO1O/MdhmcHeUkCSGJEIriQIByoIwEAOw==
}
image create photo ::domtree::collapse -data {R0lGODlhEAAQALIAAAAAAAAAMwAAZgAAmQAAzAAA/wAzAAAzMyH5BAUAAAYA
LAAAAAAQABAAggAAAGZmzIiIiLu7u5mZ/8zM/////wAAAAMlaLrc/jDKSRm4
OAMHiv8EIAwcYRKBSD6AmY4S8K4xXNFVru9SAgAh/oBUaGlzIGFuaW1hdGVk
IEdJRiBmaWxlIHdhcyBjb25zdHJ1Y3RlZCB1c2luZyBVbGVhZCBHSUYgQW5p
bWF0b3IgTGl0ZSwgdmlzaXQgdXMgYXQgaHR0cDovL3d3dy51bGVhZC5jb20g
dG8gZmluZCBvdXQgbW9yZS4BVVNTUENNVAAh/wtQSUFOWUdJRjIuMAdJbWFn
ZQEBADs=
}
image create photo ::domtree::expand -data {R0lGODlhEAAQALIAAAAAAAAAMwAAZgAAmQAAzAAA/wAzAAAzMyH5BAUAAAYA
LAAAAAAQABAAggAAAGZmzIiIiLu7u5mZ/8zM/////wAAAAMnaLrc/lCB6MCk
C5SLNeGR93UFQQRgVaLCEBasG35tB9Qdjhny7vsJACH+gFRoaXMgYW5pbWF0
ZWQgR0lGIGZpbGUgd2FzIGNvbnN0cnVjdGVkIHVzaW5nIFVsZWFkIEdJRiBB
bmltYXRvciBMaXRlLCB2aXNpdCB1cyBhdCBodHRwOi8vd3d3LnVsZWFkLmNv
bSB0byBmaW5kIG91dCBtb3JlLgFVU1NQQ01UACH/C1BJQU5ZR0lGMi4wB0lt
YWdlAQEAOw==
}


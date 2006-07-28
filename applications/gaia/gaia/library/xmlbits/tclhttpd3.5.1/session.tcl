# session.tcl -- 
# Session management support.
#
# A session is implemented as a safe slave interpreter that holds its state.
# Creating a session with Session_Create returns a 4 character ID.
# The idea is that form data will have either
# session=new or session=XXXX
# Use Session_Match to find and/or create a session based on query data.
# Use Session_Destroy to delete one session, and Session_Reap to
# clean up "old" sessions.
#
# A session has a type, which is used to automatically create aliases for
# the slave.  If the type is Foo, then every Tcl procedure named Foo_*
# in the master will be created as an alias.  The Foo_ prefix gets
# stripped off the alias name in the slave.
#
# Certain first arguments are treated specially for the aliases:
# if the first argument is "session", then the alias automatically
# gets called with the current session ID as the first argument.
# if the first argument is "interp", then the alias automatically
# gets called with the interp name as the first argument.
# (From a session you get also get the interp name).
#
# In addition, a few aliases are defined for all types:
# session - returns the session identifier
# sequence - returns an increasing sequence number used to chain together
# 	pages and detect bookmarks and "backs" that screw things up.
# group - set or get the current session "group".
# value - get a value from the current "group", or a default value
#
# Session state can be organized in "groups", which are just Tcl arrays
# in the safe interpreters.  The master keeps track of the current group
# for a session, and the slave can change the group and query group
# values with the "value" alias.  There is no standard for setting
# group values, but typically query data is copied into them by
# the module that uses the sessions.
#
# Stephen Uhler  (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::session 1.0

# use long session IDs
if {![info exists Session(short)]} {
    set Session(short) 0
}

# how long do session cookies last?
if {![info exists Session(expires)]} {
    set Session(expires) "now + 10 years"	;# should be long enough
}

# create a directory for saved sessions
if {![info exists Session(dir)]} {
    # I'm not keen on this default location, as it could
    # be exposed to URL fetches
    set Session(dir) [file join [Doc_Root] .sessions]
}

# if an MD5 package is available, we use it to make
# an unforgeable session ID, otherwise default to 
# the old 4-digit session ID
if {($Session(short) == 1) || [catch {package require md5}]} {
    # we have no usable md5 or have elected short session IDs
    proc SessionGenId {} {
	return [randomx]
    }
} else {
    # generate a long and unforgeable session id
    proc SessionGenId {} {
	return [md5hex [randomx]]
    }
}

proc Session_Authorized {id} {
    upvar #0 Session:$id session
    if ![info exists session(interp)] {
	set interp [SessionCreate $id]
	SessionAuthorizedAliases $interp $id
    }
    return $session(interp)
}

proc SessionAuthorizedAliases {interp id} {
    upvar #0 Session:$id session
    interp alias $interp require {} Session_Require $id
    interp alias $interp exit {} Session_Destroy $id
}

proc Session_Require {id tag} {
    upvar #0 Session:$id session
    if ![info exists session(init)] {
	if ![iscommand ${tag}_Init] {
	    set html "
		<h4>No ${tag}_Init proc</h4>
	    "
	} else {
	    set html [${tag}_Init Session:$id]
	}
    } else {
	set html "<-- Session $id -->"
    }
    return $html
}
proc Dummy_Init {name} {
    upvar #0 $name session
    return "<-- Dummy_Init was here -->"
}

# Create a new session as a safe slave interpreter.
# Populate it with the useful aliases.
#   Type:  An arbitrary session type, used for automatic alias creation.

proc Session_Create {type {isSafe 1}} {

    # Pick a unique session id, create the interpreter and global state.

    while {[info globals *[set id [SessionGenId]]*] != ""} {}
    Session_CreateWithID $type $id $isSafe
}

proc Session_CreateWithID {type id {isSafe 1}} {
    set interp [SessionCreate $id $isSafe]
    SessionTypeAliases $interp $id $type
}

proc SessionCreate {id {isSafe 1}} {
    upvar #0 Session:$id session
    if {$isSafe} then {
	set interp [interp create -safe interp$id]
    } else {
        set interp [interp create interp$id]
    }
    set session(start) [clock seconds]
    set session(current) $session(start)
    set session(count) 0
    set session(interp) $interp
    return $interp
}

proc SessionTypeAliases {interp id type} {
    upvar #0 Session:$id session
    set session(type) $type

    # Set up the  document specific aliases for this interp  This
    # looks for all procedures in the master interpreter that start
    # with "<type>_".  Since some of them might be auto-loaded, look
    # in the auto index array.  The names get stored in an array to
    # remove duplicates.

    global auto_index
    foreach name "[info commands ${type}_*] [array names auto_index ${type}_*]" {
    	set procs($name) {}
    }

    # Look at the *name* of the first argument to each alias-able proc.
    # Some names are treated specally.

    foreach proc [array names procs] {
	regexp -- ${type}_(.*) $proc {} alias
	catch {lindex [info args $proc] 0} arg0
	switch -- $arg0 {
	    session {interp alias $interp $alias {} $proc $id}
	    interp  {interp alias $interp $alias {} $proc $interp}
	    default {interp alias $interp $alias {} $proc}
	}
    }

    # Set up the common aliases for all session interpreters.

    interp alias $interp session {} Session_Session $id
    interp alias $interp sequence {} Session_Sequence $id
    interp alias $interp group {} Session_Variable $id group
    interp alias $interp value {} Session_Value $id

    return $id
}

# Destroy all sessions older than a certain age (in seconds)
#    age:  time (in seconds) since the most recent access
#    type: a regexp to mach session types with (defaults to all)

proc Session_Reap {age {type .*}} {
    foreach id [info globals Session:*] {
	upvar #0 $id session
	set old [expr {[clock seconds] - $age}]
	if {[regexp -- $type $session(type)] && $session(current) < $old} {
	    catch {interp delete $session(interp)}
	    Stderr "Reaping session $id"
	    unset session
	}
    }
}

# Destroy a single session

proc Session_Destroy {id} {
    upvar #0 Session:$id session
    if {[info exists session]} {
	interp delete $session(interp)
	unset session

	global Session
	set sfile [file join $Session(dir) ${id}.sess]
	if {[file exists $sfile]} {
	    # delete the session save file
	    file delete $sfile
	}

	return 1
    } else {
    	return 0
    }
}


# Find the correct session, and return the proper interp or error.
# This is identical to Session_Match, but uses cookies to store
# session id.
# If the session is "new", then create a new one.
# - query: optional alist containing the form and/or url query
# - type:  The type of this session
# - error_name:  The variable holding the error result (if any)

proc Session_Cookie {{querylist {}} {type {}} {error_name error} {isSafe 1}} {
    upvar $error_name error
    global Httpd

    # fetch cookies pertaining to session
    set old 0
    foreach {key var} {session session session_sequence sequence} {
	set x [Cookie_GetSock $Httpd(currentSocket) ${type}_$key]
	if {$x != ""} {
	    incr old
	    lappend querylist $var [lindex $x 0]
	}
    }

    # try to find an existing or saved session
    set error ""
    array set query $querylist
    set id [Session_Match [array get query] $type error $isSafe]

    # we must have lost the old session. lose the cookies and try again
    if {$error == "Session: Invalid session id."} {
	set query(session) new
	set error ""
	set id [Session_Match [array get query] $type error $isSafe]
	set old 0
    }

    # set the session cookies
    if {!$old} {
	global Session
	upvar #0 Session:$id session

	foreach {key var} {session id
	    session_sequence session(sequence)
	    session_type session(type)} {
	    if {[info exists $var]} {
		Cookie_Set -expires $Session(expires) -path / -name ${type}_$key -value [set $var]
	    }
	}
    }

    # we accept an additional session_cmd argument to manipulate sessions
    if {[info exists query(session_cmd)]} {
	switch $query(session_cmd) {
	    save {
		Session_Save $id
	    }
	}
    }

    return $id
}

# Destroy the session cookie for this type
# note, this won't take immediate effect,
# you may have to reload the page to get the new cookies
proc Session_CookieDestroy {{type {}}} {
    foreach key {session session_sequence} {
	Cookie_Unset ${type}_$key -path /
    }
}

# Save session state in a file under $Session(dir),
# for later reconstruction
proc Session_Save {id} {
    # see if there's a session to be instantiated
    global Session
    upvar #0 Session:$id session

    # create a session save file, write state
    set sfile [file join $Session(dir) ${id}.sess]
    file mkdir $Session(dir)
    set fd [open $sfile w]
    puts $fd $session(type)
    puts $fd [interp issafe interp$id]
    puts $fd [array get session]

    # add Self Vars
    set vars {}
    set arrays {}
    foreach sv [interp eval $session(interp) info vars] {
	switch -regexp -- $sv {
	    tcl_* -
	    env -
	    errorCode -
	    errorInfo -
	    arg[cv] -
	    auto_* {}
	    default {
		if {[array exists $sv]} {
		    lappend arrays $sv [interp eval $session(interp) array get $sv]
		} else {
		    lappend vars $sv [interp eval $session(interp) set $sv]
		}
	    }
	}
    }
    puts $fd $vars
    puts $fd $arrays
	
    close $fd
}

# Find the correct session, and return the proper interp or error.
# If the session is "new", then create a new one.
# - query: The array containing the form and/or url query
# - type:  The type of this session
# - error_name:  The variable holding the error result (if any)

proc Session_Match {querylist {type {}} {error_name error} {isSafe 1}} {
    upvar $error_name error

    # Check the session informatioin provided in the query data.

    if [catch {
	array set query $querylist
    }] {
	Stderr "Bogus querylist\n$querylist"
    }

    if {![info exists query(session)]} {
	set error "Session: no session id provided."
	return {}
    }

    set id $query(session)
    if {$id == "new"} {
	set id [Session_Create $type $isSafe]
    } elseif {[regexp "kill(.+)" $id x id]} {
	Session_Destroy $id
	set id [Session_Create $type $isSafe]
    } elseif {[regexp "save(.+)" $id x id]} {
	Session_Save $id
    }

    upvar #0 Session:$id session
    if {![array exists session]} {
	# see if there's a session to be instantiated
	global Session

	set sfile [file join $Session(dir) ${id}.sess]
	if {[file exists $sfile]} {
	    # open a session save file, read state
	    set fd [open $sfile r]
	    foreach {type isSafe sarray vars arrays} [split [read $fd] \n] break
	    close $fd

	    # create the interpreter
	    Session_CreateWithID $type $id $isSafe
	    array set session $sarray	;# set session vars
	    foreach {var val} $vars {
		interp eval $session(interp) [list set $var $val]
	    }
	    foreach {var val} $arrays {
		interp eval $session(interp) [list array set $var $val]
	    }
	} else {
	    set error "Session: Invalid session id."
	    return {}
	}
    }

    if {$type != {} && $type != $session(type)} {
    	set error "Session: Invalid session type."
    	return $id
    }

    # Check sequence number (if any).

    if {[info exists session(sequence)]} {
    	if {![info exists query(sequence)]} {
	    set error "Session: Sequence number required, not provided."
	    return $id
    	}
    	if {$query(sequence) != $session(sequence)} {
	    set error "Session: Sequence number invalid."
	    return $id
    	}
    	unset session(sequence)
    }

    # Update the session access time and count, and return the session id.

    set session(current) [clock seconds]
    incr session(count)
    return $id
}

# Import variables from a global array into the local scope.
#  valid:  The array containing the legal values to import.  If valid
#          is {}, then all names in "array" will be imported.
#  array:  The name of the global array  in "Interp" to import from.
#  Interp: The interpreter to import it from.

proc Session_Import {valid array {interp {}}} { 
    upvar $valid ok
    foreach {name value} [interp eval $interp [list array get $array]] {
    	if {$valid == {} || [info exists ok(-$name)]} {
	    upvar $name var
	    set var $value
	}
    }
}

###################################
# Common Aliases for interpreters

# Return the (constant) session id.  The id is a constant created at 
# alias time.

proc Session_Session {arg} {
    return $arg
}    

# set up a sequence number.  Id is the session id, passed in at alias time.

proc Session_Sequence {id} {
    upvar #0 Session:$id session
    set session(sequence)  $session(count)
}

# Set or return a session variable.  The slave alias may be set up to allow
# read access only.

proc Session_Variable {id var {value ""}} {
    upvar #0 Session:$id state
    if {$value != {}} {
    	set state($var) $value
    } 
    if {[info exists state($var)]} {
    	return $state($var)
    } else {
    	return ""
    }
}

# Get a group variable, specify a default if not set

proc Session_Value {id var {default {}}} {
    upvar #0 Session:$id session
    set group $session(group)
    if {[catch {interp eval $session(interp) set ${group}($var)} value]} {
	set value $default
    }
    return $value
}

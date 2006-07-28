# cookie.tcl
#@c Cookie support
#
# Derived from doc.tcl
# Stephen Uhler / Brent Welch (c) 1997-1998 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::cookie 1.0


# Cookie_Save
#
#@c	instruct httpd to return cookies, if any, to the browser
#
# Arguments:
#@a	interp  The interpreter in which to subst.

proc Cookie_Save {sock {interp {}}} {
    global Cookie
    if {![catch {
	interp eval $interp {uplevel #0 {set Cookie(set-cookie)}}
    } cookie]} {
	foreach c $cookie {
	    Httpd_SetCookie $sock $c
	}
	interp eval $interp {uplevel #0 {unset Cookie(set-cookie)}}
    }
}

# Cookie_Get
#
#@c	Return a *list* of cookie values, if present, else ""
#@c	It is possible for multiple cookies with the same key
#@c	to be present, so we return a list.
#@c     WARNING: This uses the environment HTTP_COOKIE, which can
#@c     get stomped on if a request reenters the event loop
#@c	See also Cookie_GetSock that doesn't have this problem
#
# Arguments:
#@a	cookie	The name of the cookie (the key)
# Returns:
#@r	a list of cookie values matching argument

proc Cookie_Get {cookie} {
    global env
    set result ""
    if {[info exist env(HTTP_COOKIE)]} {
	set rawcookie $env(HTTP_COOKIE)
    } else {
	# Try to find the connection
	if {[info exists env(HTTP_CHANNEL)]} {
	    upvar #0 Httpd$env(HTTP_CHANNEL) data
	    if {[info exist data(mime,cookie)]} {
		set rawcookie $data(mime,cookie)
	    }
	}
    }
    if {[info exist rawcookie]} {
	foreach pair [split $rawcookie \;] {
	    lassign [split [string trim $pair] =] key value
	    if {[string compare $cookie $key] == 0} {
		lappend result $value
	    }
	}
    }
    return $result
}

# Cookie_GetSock
#
#@c	Return a *list* of cookie values, if present, else ""
#@c	It is possible for multiple cookies with the same key
#@c	to be present, so we return a list.
#@c     This always gets the cookie state associated with the specified
#@c     socket, unlike Cookie_Get that looks at the environment.
#
# Arguments:
#@a	cookie	The name of the cookie (the key)
#@a	sock	A handle on the socket connection
# Returns:
#@r	a list of cookie values matching argument

proc Cookie_GetSock {sock cookie} {
    upvar #0 Httpd$sock data
    set result ""
    set rawcookie ""
    if {[info exist data(mime,cookie)]} {
        set rawcookie $data(mime,cookie)
    }
    foreach pair [split $rawcookie \;] {
        lassign [split [string trim $pair] =] key value
        if {[string compare $cookie $key] == 0} {
            lappend result $value
        }
    }
    return $result
}

# Cookie_Make
#
#$c	make a cookie from name value pairs
#
# Arguments:
#	args	Name value pairs, where the names are:
#@a		-name	Cookie name
#@a		-value	Cookie value
#@a		-path	Path restriction
#@a		-domain	domain restriction
#@a		-expires	Time restriction
#@r	a formatted cookie

proc Cookie_Make {args} {
    array set opt $args
    set line "$opt(-name)=$opt(-value) ;"
    foreach extra {path domain} {
	if {[info exist opt(-$extra)]} {
	    append line " $extra=$opt(-$extra) ;"
	}
    }
    if {[info exist opt(-expires)]} {
	switch -glob -- $opt(-expires) {
	    *GMT {
		set expires $opt(-expires)
	    }
	    default {
		set expires [clock format [clock scan $opt(-expires)] \
			-format "%A, %d-%b-%Y %H:%M:%S GMT" -gmt 1]
	    }
	}
	append line " expires=$expires ;"
    }
    if {[info exist opt(-secure)]} {
	append line " secure "
    }
    return $line
}


# Cookie_Set
#
#$c	Set a return cookie
#
# Arguments:
#	args	Name value pairs, where the names are:
#@a		-name	Cookie name
#@a		-value	Cookie value
#@a		-path	Path restriction
#@a		-domain	domain restriction
#@a		-expires	Time restriction

proc Cookie_Set {args} {
    global Cookie
    lappend Cookie(set-cookie) [eval Cookie_Make $args]
}


# Cookie_UnSet
#
#$c	Unset a return cookie
#
# Arguments:
#	args	Name value pairs, where the names are:
#@a		-name	Cookie name
#@a		-path	Path restriction
#@a		-domain	domain restriction
proc Cookie_Unset {name args} {
    Httpd_RemoveCookies [Httpd_CurrentSocket] name
    Cookie_Set -name $name -value "" -expires [clock format [clock scan "last year"] -format "%A, %d-%b-%Y %H:%M:%S GMT" -gmt 1]
}

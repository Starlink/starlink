# direct.tcl
#
# Support for application-direct URLs that result in Tcl procedures
# being invoked inside the server.
#
# Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::direct 1.1

# Direct_Url
#	Define a subtree of the URL hierarchy that is implemented by
#	direct Tcl calls.
#
# Arguments
#	virtual The name of the subtree of the hierarchy, e.g., /device
#	prefix	The Tcl command prefix to use when constructing calls,
#		e.g. Device
#	inThread	True if this should be dispatched to a thread.
#
# Side Effects
#	Register a prefix

proc Direct_Url {virtual {prefix {}} {inThread 0}} {
    global Direct
    if {[string length $prefix] == 0} {
	set prefix $virtual
    }
    set Direct($prefix) $virtual	;# So we can reconstruct URLs
    Url_PrefixInstall $virtual [list DirectDomain $prefix] $inThread
}

# Direct_UrlRemove
#       Remove a subtree of the URL hierarchy that is implemented by
#       direct Tcl calls.
#
# Arguments
#       prefix  The Tcl command prefix used when constructing calls,
#
# Side Effects
#
       
proc Direct_UrlRemove {prefix} {
    global Direct
    catch { Url_PrefixRemove $Direct($prefix) }
    catch { unset Direct($prefix) }
}
        
# Main handler for Direct domains (i.e. tcl commands)
# prefix: the Tcl command prefix of the domain registered with Direct_Url 
# sock: the socket back to the client
# suffix: the part of the url after the domain prefix.
#
# This calls out to the Tcl procedure named "$prefix$suffix",
# with arguments taken from the form parameters.
# Example:
# Direct_Url /device Device
# if the URL is /device/a/b/c, then the Tcl command to handle it
# should be
# proc Device/a/b/c
# You can define the content type for the results of your procedure by
# defining a global variable with the same name as the procedure:
# set Device/a/b/c text/plain
#  The default type is text/html

proc DirectDomain {prefix sock suffix} {
    global Direct
    global env
    upvar #0 Httpd$sock data

    # Set up the environment a-la CGI.

    Cgi_SetEnv $sock $prefix$suffix

    # Prepare an argument data from the query data.

    Url_QuerySetup $sock
    set cmd [Direct_MarshallArguments $prefix $suffix]
    if {$cmd == ""} {
	Doc_NotFound $sock
	return
    }

    # Eval the command.  Errors can be used to trigger redirects.

    set code [catch $cmd result]

    set type text/html
    upvar #0 $prefix$suffix aType
    if {[info exist aType]} {
	set type $aType
    }

    DirectRespond $sock $code $result $type
}

# Direct_MarshallArguments --
#
#	Use the url prefix, suffix, and cgi values (set with the
#	ncgi package) to create a Tcl command line to invoke.
#
# Arguments:
# 	prefix		The Tcl command prefix of the domain registered 
#			with Direct_Url.
#	suffix		The part of the url after the domain prefix.
#
# Results:
#	Returns a Tcl command line.
#
# Side effects:
#	If the prefix and suffix do not map to a Tcl procedure,
#	returns empty string.

proc Direct_MarshallArguments {prefix suffix} {
    global Direct

    set cmd $prefix$suffix
    if {![iscommand $cmd]} {
	return
    }

    # Compare built-in command's parameters with the form data.
    # Form fields with names that match arguments have that value
    # passed for the corresponding argument.
    # Form fields with no corresponding parameter are collected into args.

    set cmdOrig $cmd
    set params [info args $cmdOrig]
    foreach arg $params {
	if {[ncgi::empty $arg]} {
	    if [info default $cmdOrig $arg value] {
		lappend cmd $value
	    } elseif {[string compare $arg "args"] == 0} {
		set needargs yes
	    } else {
		lappend cmd {}
	    }
	} else {
	    
	    # The original semantics for Direct URLS is that if there
	    # is only a single value for a parameter, then no list
	    # structure is added.  Otherwise the parameter gets a list
	    # of all values.

	    set vlist [ncgi::valueList $arg]
	    if {[llength $vlist] == 1} {
		lappend cmd [ncgi::value $arg]
	    } else {
		lappend cmd $vlist
	    }
	}
    }
    if [info exists needargs] {
	foreach {name value} [ncgi::nvlist] {
	    if {[lsearch $params $name] < 0} {
		lappend cmd $name $value
	    }
	}
    }
    return $cmd
}

# DirectRespond --
#
#	This function returns the result of evaluating the direct
#	url.  Usually, this involves returning a page, but a redirect
#	could also occur.
#
# Arguments:
# 	sock	The socket back to the client.
#	code	The return code from evaluating the direct url.
#	result	The return string from evaluating the direct url.
#	type	The mime type to use for the result.  (Defaults to text/html).
#	
#
# Results:
#	None.
#
# Side effects:
#	If code 302 (redirect) is passed, calls Httpd_Redirect to 
#	redirect the current request to the url in result.
#	If code 0 is passed, the result is returned to the client.
#	If any other code is passed, an exception is raised, which
#	will cause a stack trace to be returned to the client.
#

proc DirectRespond {sock code result {type text/html}} {
    switch $code {
	0 {
	    # Fall through to Httpd_ReturnData.
	}
	302	{
	    # Redirect.

	    Httpd_Redirect $result $sock
	    return ""
	}
	default {
	    # Exception will cause error page to be returned.

	    global errorInfo errorCode
	    return -code $code -errorinfo $errorInfo -errorcode $errorCode \
		    $result
	}
    }

    # See if a content type has been registered for the URL.

    # Save any return cookies which have been set.
    # This works with the Doc_SetCookie procedure that populates
    # the global cookie array.
    Cookie_Save $sock

    Httpd_ReturnData $sock $type $result
    return ""
}

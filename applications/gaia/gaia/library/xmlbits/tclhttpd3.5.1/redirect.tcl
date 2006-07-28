# redirect.tcl
#
# Support for redirecting URLs.
# You can either do a single redirect (Redirect_Url)
# or you can redirect a whole subtree elsewhere (Redirect_UrlTree)
#
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::redirect 1.0

# Redirect_To --
#
# Trigger a page redirect
#
# Arguments:
#	newurl	The new URL
#
# Results:
#	None
#
# Side Effects:
#	Raises a special error that is caught by Url_Unwind

proc Redirect_To {newurl} {
    return -code error \
	    -errorcode  [list HTTPD_REDIRECT $newurl] \
	    "Redirect to $newurl"
}

# Redirect_Self --
#
#	Like Redirect_To, but to a URL that is relative to this server.
#
# Arguments:
#	newurl	Server-relative URL
#
# Results:
#	None
#
# Side Effects:
#	See Redirect_To

proc Redirect_Self {newurl} {
    set thispage [ncgi::urlStub]
    set thisurl [Httpd_SelfUrl $thispage]
    set newurl [uri::resolve $thisurl $newurl]
    Redirect_To $newurl
}


# Redirect_Init
#
#	Initialize the redirect module.
#
# Arguments:
#	url	(optional) Direct URL for redirect control.
#
# Side Effects:
#	Registers access hook to implement redirects.
#	May register direct domain

proc Redirect_Init {{url {}}} {
    if {[string length $url]} {
	Direct_Url $url Redirect
    }
    # The [list] makes the eval in Url_Dispatch run slightly faster
    Url_AccessInstall [list RedirectAccess]

    # Load the default redirect file
    Redirect/reload
}

# Redirect_UrlTree
#
#	Map a whole URL hierarchy to a new place
#
# Arguments:
#	old	Old location, (e.g., /olddir)
#	new	New location, (e.g., /newdir)
#
# Side Effects:
#	Future requests to old/a/b/c will redirect to new/a/b/c

proc Redirect_UrlTree {old new} {
    Url_PrefixInstall $old [list RedirectDomain $new]
}

# RedirectDomain
#
#	Set up a domain that redirects requests elsewhere
#	To use, make a call like this:
#
#	Url_PrefixInstall /olddir [list RedirectDomain /newdir]
#
# Arguments:
#	prefix	The Prefix of the domain
#	url	The other URL to which to redirect
#
# Results:
#	This always raises a redirect error

proc RedirectDomain {url sock suffix} {
    set newurl $url$suffix
    Httpd_Redirect $newurl $sock
}

# Redirect_Url
#
#	Redirect a single URL to another, fully-qualified URL.
#
# Arguments:
#	old	Old location, (e.g., /olddir/file.html)
#	new	New location, (e.g., http://www.foo.bar/newdir/newfile.html)
#
# Side Effects:
#	Future requests to $old will redirect to $new

proc Redirect_Url {old new} {
    global Redirect
    set Redirect($old) $new
}

# Redirect_UrlSelf
#
#	Redirect a single URL to another location on the same server.
#
# Arguments:
#	old	Old location, (e.g., /olddir/file.html)
#	new	New location, (e.g., /newdir/newfile.html)
#
# Side Effects:
#	Future requests to $old will redirect to $new

proc Redirect_UrlSelf {old new} {
    global RedirectSelf

    # Cannot make the "self" computation until we have
    # a socket and know the protocol and server name for sure

    set RedirectSelf($old) $new
}

# RedirectAccess
#
#	This is invoked as an "access checker" that will simply
#	redirect a URL to another location.
#
# Arguments:
#	sock	Current connection
#	url	The url of the connection
#
# Results:
#	Returns "denied" if it triggered a redirect.  This stops URL processing.
#	Returns "skip" otherwise, so other access control checks can be made.

proc RedirectAccess {sock url} {
    global Redirect
    global RedirectSelf

    if {[info exist RedirectSelf($url)]} {
	
	# Note - this is not an "internal" redirect, but in this case
	# the server simply qualifies the url with its own name

	Httpd_RedirectSelf $RedirectSelf($url) $sock
	return denied
    }
    if {[info exist Redirect($url)]} {
	Httpd_Redirect $Redirect($url) $sock
	return  denied
    }
    return skip
}

# Redirect/reload
#
#	Direct URL to reload the redirect configuration file.
#
# Arguments:
#	none
#
# Side Effects:
#	Sources "redirect" from the document root.

proc Redirect/reload {} {
    global Doc
    set path [file join $Doc(root) redirect]
    if { ! [file exists $path]} {
	return
    }
    source $path
    set html "<h3>Reloaded redirect file</h3>"
    append html [Redirect/status]
    return $html
}

# Redirect/status
#
#	Display the Redirection tables
#
# Arguments:
#	none
#
# Results:
#	An HTML table

proc Redirect/status {} {
    global Redirect
    global RedirectSelf
    global Url	;# hack alert

    append html "<h3>Redirect Table</h3>\n"
    append html "<table>\n"
    append html "<tr><th colspan=2>Single URLs</th></tr>\n"
    foreach old [lsort [array names RedirectSelf]] {
	append html "<tr><td>$old</td><td>$RedirectSelf($old)</td></tr>\n"
    }
    foreach old [lsort [array names Redirect]] {
	append html "<tr><td>$old</td><td>$Redirect($old)</td></tr>\n"
    }
    append html "<tr><th colspan=2>URL Subtrees</th></tr>\n"
    foreach prefix [lsort [array names Url command,*]] {
	if {[string match RedirectDomain* $Url($prefix)]} {
	    set new [lindex $Url($prefix) 1]
	    regsub command, $prefix {} prefix
	    append html "<tr><td>$prefix</td><td>$new</td></tr>\n"
	}
    }
    append html </table>\n
    return $html
}

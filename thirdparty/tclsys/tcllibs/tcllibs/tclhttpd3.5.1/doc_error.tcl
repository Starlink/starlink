# doc_error.tcl
#@c handlers for server errors and doc-not-found cases.
#
#
# Derived from doc.tcl
# Stephen Uhler / Brent Welch (c) 1997-1998 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::doc_error 1.0

package require httpd::subst

# Doc_NotFoundPage --
#
#@c Register a file not found error page.
#@c This page always gets "subst'ed, but without the fancy
#@c context of the ".tml" pages.
#@c
# Arguments:
#@a	virtual	The URL of the not-found page, e.g., /notfound.html
#
# Results:
#@r	None
#
# Side Effects:
#@e	Sets the not-found page.

proc Doc_NotFoundPage { virtual } {
    global Doc
    set Doc(page,notfound) [Doc_Virtual {} {} $virtual]
}

# Doc_ErrorPage --
#
#@c Register a server error page.
#@c This page always gets "subst'ed"
#
# Arguments:
#@a	virtual	The URL of the error page, e.g., /error.html
#
# Results:
#@r	None
#
# Side Effects:
#@e	Sets the error page.

proc Doc_ErrorPage { virtual } {
    global Doc
    set Doc(page,error) [Doc_Virtual {} {} $virtual]
}

# Doc_NotFound --
#
#@c	Called when a page is missing.  This looks for a handler page
#@c	and sets up a small amount of context for it.
#
# Arguments:
#@a	sock	The socket connection.
#
# Results:
#@r	None
#
# Side Effects:
#@e	Returns a page.

proc Doc_NotFound { sock } {
    global Doc Referer
    upvar #0 Httpd$sock data
    CountName $data(url) notfound
    set Doc(url,notfound) $data(url)	;# For subst
    if {[info exists data(mime,referer)]} {

	# Record the referring URL so we can track down
	# bad links

	lappendOnce Referer($data(url)) $data(mime,referer)
    }
    DocSubstSystemFile $sock notfound 404 [protect_text $Doc(url,notfound)]
}

# Doc_Error --
#
#@c	Called when an error has occurred processing the page.
#
# Arguments:
#@a	sock	The socket connection.
#	ei	errorInfo
#
# Results:
#@r	None
#
# Side Effects:
#@e	Returns a page.

proc Doc_Error { sock ei } {
    global Doc
    upvar #0 Httpd$sock data
    # Could have been reset!!!
    catch {
	set Doc(errorUrl) $data(url)
	set Doc(errorInfo) $ei	;# For subst
	CountName $Doc(errorUrl) errors
    }
    if {![info exists data(error_hook)] || [catch {$data(error_hook) $sock}]} {
	DocSubstSystemFile $sock error 500 [protect_text $ei]
    }
}

# DocSubstSystemFile --
#
#	Simple template processor for notfound and error pages.
#
# Arguments:
#	sock	The socket connection
#	key	Either "notfound" or "error"
#	code	HTTP code
#	extra 	Optional string to include in return page.
#	interp  Interp to use for Subst.
#
# Results:
#	None
#
# Side Effects:
#	Returns a page.

proc DocSubstSystemFile {sock key code {extra {}} {interp {}}} {
    global Doc env
    if {![info exists Doc(page,$key)]} {
	set path [Doc_Virtual {} {} /$key.html]
	if {[file exists $path]} {
	    set Doc(page,$key) $path
	}
    }
    if {![info exists Doc(page,$key)] || 
	[catch {Subst_ReturnFile $sock $Doc(page,$key) $interp} err]} {
	if {[info exists err]} {
	    Log $sock DocSubstSystemFile $err
	}
	Httpd_Error $sock $code $extra
    }
}

# Doc_ErrorInfo --
#
#@c Return the error information raised by this page
#
# Arguments:
#
# Results:
#@r	$Doc(errorInfo)
#
# Side Effects:
#@e	None

proc Doc_ErrorInfo {} {
    global Doc
    return $Doc(errorInfo)
}

# Doc_UrlNotFound --
#
#@c Return the url which was not found (in notfound handler)
#
# Arguments:
#
# Results:
#@r	$Doc(url,notfound)
#
# Side Effects:
#@e	None

proc Doc_UrlNotFound {} {
    global Doc
    return $Doc(url,notfound)
}

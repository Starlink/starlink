# subst.tcl
#@c Subst support
#
# Derived from doc.tcl
# Stephen Uhler / Brent Welch (c) 1997-1998 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::subst 1.0

# Subst_ReturnFile --
#
# Subst a file and return the result to the HTTP client.
# Note that ReturnData has no Modification-Date so the result is not cached.
#
# Arguments:
#	sock	The socket connection.
#	path	The template file pathname.
#	interp 	The Tcl intepreter in which to subst.
#
# Results:
#	None
#
# Side Effects:
#	Returns a page to the client.

proc Subst_ReturnFile {sock path {interp {}}} {
    Httpd_ReturnData $sock text/html [Subst_File $path $interp]
}

# SubstCleanScope --
#
# Substitute the data in this clean (no vars set) scope.
#
# Arguments:
#	_html_data	The data to substitute.
#
# Results:
#	The subst'ed data.
#
# Side Effects:
#	None

proc SubstCleanScope {_html_data} {
    return [subst $_html_data]
}

# Subst_Scope --
#
# When processing templates in the current interpreter, decide whether to
# use the global or local scope (that of DocSubst) to process templates.
#
# Arguments:
#	scope	0 means global and non-zero means local.
#
# Results:
#	None
#
# Side Effects:
#	Sets the scope for all Doc domain substs.

proc Subst_Scope {scope} {
    global Subst
    set Subst(templateScope) $scope
}
if {![info exists Subst(templateScope)]} {
    set Subst(templateScope) 0
}

# SubstFile --
#
# Subst a file in an interpreter context.  If no interp is given, use the
# current interp.  If using the current interp, use the scope
# variable to decide whether to use the global or current scope.
#
# Arguments:
#	path	The file pathname of the template.
#	interp  The interpreter in which to subst.
#
# Results:
#	The subst'ed page.
#
# Side Effects:
#	None

proc SubstFile {path {interp {}}} {
    global Subst

    set in [open $path]
    set script [read $in]
    close $in

    if {[string length $interp] == 0} {
        # Substitution occurs in the current interp.
        if {$Subst(templateScope) == 0} {
            # Substitution occurs at the global level.
            set result [uplevel #0 [list subst $script]]
        } else {
            # Substitution occurs at a clean level, one-off from global.
            set result [uplevel [list SubstCleanScope $script]]
        }
    } else {
        # Substitution occurs in the given interp.
        set result [interp eval $interp [list subst $script]]
    }

    # Perform the post-processing of the output, as installed via Subst_Install.
    foreach hook $Subst(substHooks) {
        set result [$hook $result]
    }
    return $result
}

# Subst_File --
#
# Subst a file or directory in an interpreter context.
# As SubstFile except that a path which is a directory is evaluated
# by evaluating a file $path/index.tml, and returning that as the substituted
# value of the $path directory.
#
proc Subst_File {path {interp {}}} {
    global Subst

    switch [file type $path] {
	file -
	link {
	    return [uplevel 1 [list SubstFile $path $interp]]
	}
	directory {
	    return [uplevel 1 [list SubstFile [file join $path index.tml] $interp]]
	}
	default {
	    error "Can't process [file type $path] files."
	}
    }
}

# Subst_Install
#
#	Install a subst hook.  Each installed hook will be performed over the
#	substituted template.
#
# Arguments
#	proc	This is a command prefix that is invoked with one additional
#		arguments to process:
#			text	The text to process.
#		The subst hook returns the HTML that will be returned by the server.
#
# Results:
#	None
#
# Side Effects
#	Save the subst hook.

proc Subst_Install {proc} {
    global Subst
    if {[lsearch $Subst(substHooks) $proc] < 0} {
	lappend Subst(substHooks) $proc
    }
    return
}

if {![info exist Subst(substHooks)]} {
    set Subst(substHooks) {}
}

# Doc_application/x-tcl-auth --
#
# Like tcl-subst, but a basic authentication cookie is used for session state
#
# Arguments:
#	path	The file pathname.
#	suffix	The URL suffix.
#	sock	The socket connection.
#
# Results:
#	None
#
# Side Effects:
#	Returns a page to the client.

proc Doc_application/x-tcl-auth {path suffix sock} {
    upvar #0 Httpd$sock data

    if {![info exists data(session)]} {
	Httpd_RequestAuth $sock Basic "Random Password"
	return
    }
    set interp [Session_Authorized $data(session)]

    # Need to make everything look like a GET so the Cgi parser
    # doesn't read post data from stdin.  We've already read it.

    set data(proto) GET

    Doc_application/x-tcl-subst $path $suffix $sock
}

# Doc_application/x-tcl-subst --
#
# Tcl-subst a template that mixes HTML and Tcl.
# This subst is just done in the context of the specified
# interpreter with not much other support.
# See x-tcl-template for something more sophisticated
#
# Arguments:
#	path	The file pathname.
#	suffix	The URL suffix.
#	sock	The socket connection.
#	interp	The interp to use for subst'ing.
#
# Results:
#	None
#
# Side Effects:
#	Sets the env array in interp and calls Subst_ReturnFile.

proc Doc_application/x-tcl-subst {path suffix sock {interp {}}} {
    upvar #0 Httpd$sock data

    Cgi_SetEnv	$sock $path pass
    interp eval $interp [list uplevel #0 [list array set env [array get pass]]]
    Subst_ReturnFile $sock $path $interp
}


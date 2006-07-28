# fallback.tcl
#@c Fallback does "content negotation" if a file isn't found
#@c look around for files with different suffixes but the same root.
#
# NOTE: This feature is probably more trouble than it is worth.
# It was originally used to be able to choose between different
# iamge types (e.g., .gif and .jpg), but is now also used to
# find templates (.tml files) that correspond to .html files.
#
#
# Derived from doc.tcl
# Stephen Uhler / Brent Welch (c) 1997-1998 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::fallback 1.0

package require httpd::mtype

# Fallback_ExcludePat --
#
# Define a pattern of files names to exclude in Fallback
#
# Arguments:
#	patlist	A glob pattern of files to avoid when playing
#		games in FallBack to find an alternative file.
#
# Results:
#	None
#
# Side Effects:
#	Sets the exclude pattern.

proc Fallback_ExcludePat {patlist} {
    global Fallback
    set Fallback(excludePat) $patlist
}
if {![info exists Fallback(excludePat)]} {
    set Fallback(excludePat) {*.bak *.swp *~}
}

# Fallback_Try
#
# Arguments:
#	prefix	The URL prefix of the domain.
#	path	The pathname we were trying to find.
#	suffix	The URL suffix.
#	sock	The socket connection.
#
# Results:
#	None
#
# Side Effects:
#	This either triggers an HTTP redirect to switch the user
#	to the correct file name, or it calls out to the template-aware
#	text/html processor.

proc Fallback_Try {virtual path suffix sock} {
    set root [file root $path]
    if {[string match */ $root]} {
	# Input is something like /a/b/.xyz
	return 0
    }

    # Here we look for files indicated by any Accept headers.
    # Most browsers say */*, but they may provide some ordering info, too.

    # First generate a list of candidate files by ignoring extension
    set ok {}
    foreach choice [glob -nocomplain $root.*] {
	
	# Filter on the exclude patterns, and make sure that we
	# don't let "foo.html.old" match for "foo.html"

	if {[string compare [file root $choice] $root] == 0 &&
		![FallbackExclude $choice]} {
	    lappend ok $choice
	}
    }

    # Now we pick the best file from the ones that matched.
    set npath [FallbackChoose [Mtype_Accept $sock] $ok]
    if {[string length $npath] == 0 || [string compare $path $npath] == 0} {

	# not found or still trying one we cannot use

	return 0
    } else {
	# A file matched, but has a different extension to that requested

	# Another hack for templates.  If the requested .html is not found,
	# and the .tml exists, ask for .html so the template is
	# processed and cached as the .html file.

	global Template
	if {[string compare $Template(tmlExt) [file extension $npath]] == 0} {
	    Doc_text/html [file root $npath]$Template(htmlExt) $suffix $sock
	    return 1
	}

	# No template matched the request, so redirect_to/offer our best match.
	# Redirect so we don't mask spelling errors like john.osterhoot

	set new [file extension $npath]
	set old [file extension $suffix]
	if {[string length $old] == 0} { 
	    append suffix $new
	} else {
	    # Watch out for specials in $old, like .html)

	    regsub -all {[][$^|().*+?\\]} $old {\\&} old
	    regsub $old\$ $suffix $new suffix
	}

	# Preserve query data when bouncing among pages.

	upvar #0 Httpd$sock data
	set url $virtual/[string trimleft $suffix /~]
	if {[info exist data(query)] && [string length $data(query)]} {
	    append url ? $data(query)
	}

	Redirect_Self $url	;# offer what we have to the client
    }
}

# FallbackExclude --
#
# This is used to filter out files like "foo.bak"  and foo~
# from the Fallback failover code
#
# Arguments:
#	name	The filename to filter.
#
# Results:
#	1	If the file should be excluded, 0 otherwise.
#
# Side Effects:
#	None

proc FallbackExclude {name} {
    global Fallback
    foreach pat $Fallback(excludePat) {
	if {[string match $pat $name]} {
	    return 1
	}
    }
    return 0
}

# FallbackChoose --
#
# Choose based first on the order of things in the Accept type list,
# then on the newest file that matches a given accept pattern.
#
# Arguments:
#	accept	The results of Mtype_Accept
#	choices	The list of matching file names.
#
# Results:
#	Name of the newest file whose mime type is most acceptable
#	to client browser.
#
# Side Effects:
#	None

proc FallbackChoose {accept choices} {
    foreach t [split $accept ,] {
	regsub {;.*} $t {} t	;# Nuke quality parameters
	set t [string trim [string tolower $t]]
	set hits {}
	foreach f $choices {
	    set type [Mtype $f]	;# mime-type for f's file extension
	    if {[string match $t $type]} {
		lappend hits $f	;# this file provides a matching mime type
	    }
	}
	set result [file_latest $hits]	;# latest file matching mime type $t
	if {[string length $result]} {
	    return $result
	}
    }
    return {}
}

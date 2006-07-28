# common.tcl --
#
#	Common code shared between tkxmllint and tkxsltproc.
#
#	The master version of this file is in the TclDOM project.
#
# Copyright (c) 2005 Explain
# http://www.explain.com.au
# Copyright (c) 2004 Zveno
# http://www.zveno.com/
#
# See the file "LICENSE" in this distribution for information on usage and
# redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# $Id$

package require http

# SetProperties --
#
#	Setup tag properties
#
# Arguments:
#	win	toplevel window
#	log	log window
#
# Results:
#	Tag properties set

proc SetProperties {win log} {

    $log tag configure timing -background #bafdff
    $log tag configure error -background #ff9d8d
    $log tag configure errorhighlight -background #cd3030
    $log tag configure related -background #ffe59f
    $log tag configure relatedhighlight -background #e8b417
    $log tag configure message -background #ffe59f
    $log tag configure log -background #b9ffd4

    return {}
}

# Browse --
#
#	Choose a file
#
# Arguments:
#	win	toplevel window
#	field	name of state variable field to update
#	args	configuration options
#
# Results:
#	Current file is set

proc Browse {win field args} {
    upvar \#0 State$win state

    set w [expr {$win == "." ? {} : $win}]

    array set opts {
	-title {Select Document}
	-type open
	-command {}
    }
    array set opts $args

    set cwd [pwd]
    if {$state(cwd) != {}} {
	set cwd $state(cwd)
    }

    switch -- $opts(-type) {
	save {
	    set fname [tk_getSaveFile -parent $win -title [mc $opts(-title)] -initialdir $cwd]
	}
	open -
	default {
	    set fname [tk_getOpenFile -parent $win -title [mc $opts(-title)] -initialdir $cwd]
	}
    }

    if {![string length $fname]} {
	return {}
    }

    set state($field) file:///$fname
    set state(cwd) [file dirname $fname]

    if {[string length $fname] && [string length $opts(-command)]} {
	uplevel #0 $opts(-command)
    }

    return {}
}

# ReadAndParseXML --
#
#	Helper procedure to read an XML document from a file
#	and parse it into a DOM tree.
#
# Arguments:
#	win	toplevel window
#	label	description of the document
#	fname	filename of document to parse
#	baseuri	base URI for document
#	timearr	name of array for timing information,
#		"start" entry must exist.
#	args	additional options
#
# Results:
#	Document read into memory.  Log messages provide feedback.
#	Returns DOM document token.

proc ReadAndParseXML {win label fname baseuri {timearr time} args} {
    upvar 1 $timearr time
    upvar \#0 State$win state

    array set opts {
	-noent 0
	-nonet 0
    }
    array set opts $args

    set state(externalentities) 0

    Feedback $win [mc "Opening $label document \"$fname\""]
    if {[string match http://* $fname]} {
	FeedbackProgress $win 0
	set state(start_download) [clock clicks -milliseconds]
	if {[catch {::http::geturl $fname \
			-command [list HTTPComplete $win] \
			-progress [list HTTPProgress $win] \
			-timeout 30000} token]} {
	    tk_messageBox -message "unable to retrieve $label document \"$fname\" due to \"$token\"" -parent $win -type ok -icon error
	    return -code error {}
	}
	::http::wait $token
	if {[::http::status $token] != "ok"} {
	    return -code error {}
	}
	set xml [::http::data $token]
	::http::cleanup $token
	set time(read) [clock clicks -milliseconds]
    } else {
	if {[catch {open $fname} ch]} {
	    tk_messageBox -message "unable to open $label document \"$fname\" due to \"$ch\"" -parent $win -type ok -icon error
	    return -code error {}
	}
	set time(open) [clock clicks -milliseconds]
	Log timing $win "Opening $label document took [expr $time(open) - $time(start)]ms\n"

	Feedback $win [mc "Reading $label document"]
	# Take note of encoding information
	set encoding {}
	gets $ch xmldecl
	set re ^[::sgml::cl $::xml::Wsp]*<\\?xml[::sgml::cl $::xml::Wsp]+(version[::sgml::cl $::xml::Wsp]*=[::sgml::cl $::xml::Wsp]*("|')[::sgml::cl ^"']+\\2)?[::sgml::cl $::xml::Wsp]*(encoding[::sgml::cl $::xml::Wsp]*=[::sgml::cl $::xml::Wsp]*("|')([::sgml::cl ^"']+)\\4)?[::sgml::cl $::xml::Wsp]*(standalone[::sgml::cl $::xml::Wsp]*=[::sgml::cl $::xml::Wsp]*("|')(yes|no)\\7)?[::sgml::cl $::xml::Wsp]*\\?>
	if {[regexp $re $xmldecl discard allversion delimiter allencoding delimiter encoding allstandalone delimiter]} {
	    if {[catch {fconfigure $ch -encoding $encoding} msg]} {
		if {[catch {fconfigure $ch -encoding [string tolower $encoding]} msg]} {
		    tk_messageBox -message "unable to read $label document \"$fname\" due to \"$msg\"" -parent $win -type ok -icon error
		    return -code error {}
		}
	    }
	}
	set xml $xmldecl\n[read $ch]
	close $ch
	# Watch out for UTF-16 documents
	if {[regexp "^(\xFF\xFE)|(\xFE\xFF)" $xml]} {
	    set xml [encoding convertfrom unicode $xml]
	}
	set time(read) [clock clicks -milliseconds]
	Log timing $win "Reading $label document took [expr $time(read) - $time(open)]ms\n"
    }

    Feedback $win [mc "Parsing $label XML"]
    if {[catch {dom::parse $xml \
	    -baseuri [uri_escape $baseuri] \
	    -defaultexpandinternalentities $opts(-noent) \
	    -externalentitycommand [list External $win]} doc]} {

	if {[string match "unable to*" $doc]} {
	    Log add $win $doc
	} else {
	    Log addXMLError $win $xml $doc
	}
	Feedback $win [mc "Parsing $label document failed"]
	after 2000 [list Feedback $win {}]
	return -code error {}
    }
    set time(parse) [clock clicks -milliseconds]
    Log timing $win "Parsing $label document took [expr $time(parse) - $time(read)]ms\n"
    set time(last) $time(parse)

    if {$state(xinclude)} {
	Feedback $win [mc "$label document XInclude processing"]
	# TODO: handle doc in slave interp
	if {[catch {dom::xinclude $doc} msg]} {
	    Log addDocError $win $doc $msg
	    Feedback $win [mc "$label document XInclude processing failed"]
	    after 2000 [list Feedback $win {}]
	}
	set time(xinclude) [clock clicks -milliseconds]
	Log timing $win "$label document XInclude took [expr $time(xinclude) - $time(last)]ms\n"
	set time(last) $time(xinclude)
    }

    return $doc
}

# External --
#
#	Handle external entity references
#
# Arguments:
#	win	toplevel window
#	name	current parser
#	baseuri	base URI of document
#	uri	system identifier of referenced entity
#	id	public identifier of referenced entity
#
# Results:
#	This reference is logged.
#	If loading of external entities is enabled then the entity is laoded as usual,
#	otherwise an empty entity is returned.

proc External {win name baseuri uri id} {
    upvar \#0 State$win state

    if {$state(nonet) &&
	([string match http:* $uri] || [string match ftp:* $uri])} {
	Log entity $win "external entity not loaded, network access not permitted: system ID \"$uri\" public ID \"$id\""
	return {}
    }

    Log entity $win "external entity reference: system ID \"$uri\" public ID \"$id\""

    incr state(externalentities)
    # resume normal loading of external entity
    return -code continue {}
}

# GetFilename --
#
#	Helper routine to retrieve resource filename
#
# Arguments:
#	win	toplevel window
#	entry	entry widget containing filename value
#	field	member of state array containing URI
#
# Results:
#	Returns filename.  If URI is not a valid file: URL,
#	returns empty string and displays message.

proc GetFilename {win entry field} {
    upvar \#0 State$win state

    set state($field) [$entry get]

    if {[catch {uri::split $state($field)} spliturl]} {
	# Try the URL as a pathname
	set fname $state($field)
	set state($field) file:///$state(field)
    } else {
	array set urlarray $spliturl
	switch -- $urlarray(scheme) {
	    http {
		set fname $state($field)
	    }
	    file {
		set fname $urlarray(path)
	    }
	    default {
		tk_messageBox -message "\"$urlarray(scheme)\" type URLs are not supported" -parent $win -type ok -icon warning
		return {}
	    }
	}
    }

    return $fname
}

# HTTPComplete --
#
#	HTTP download is finished
#
# Arguments:
#	win	toplevel window
#	token	http token
#
# Results:
#	Set progress to completion

proc HTTPComplete {win token} {
    upvar \#0 State$win state

    $state(progress) itemconfigure $state(progressbar) -state disabled
    Log timing $win "Downloading document took [expr [clock clicks -milliseconds] - $state(start_download)]ms\n"

    return {}
}

# HTTPProgress --
#
#	HTTP download is in progress
#
# Arguments:
#	win	toplevel window
#	token	http token
#	total	total number of bytes to download
#	current	number of bytes downloaded so far
#
# Results:
#	Set progress bar

proc HTTPProgress {win token total current} {
    upvar \#0 State$win state

    FeedbackProgress $win [expr ($current * 100) / $total]

    return {}
}

# Log --
#
#	Manage the log window
#
# Arguments:
#	win	toplevel window
#	args	messages to display
#
# Results:
#	Log window updated.

proc Log {method win args} {
    upvar \#0 State$win state

    set w [expr {$win == "." ? {} : $win}]

    switch -- $method {
	clear {
	    $state(messages).log configure -state normal
	    $state(messages).log delete 1.0 end
	    $state(messages).log configure -state disabled
	}
	view {
	    set what [lindex $args 0]
	    switch -- $what {
		start {
		    $state(messages).log see 1.0
		}
		end {
		    $state(messages).log see end
		}
		default {
		    return -code error "don't know how to view \"$what\""
		}
	    }
	}
	add {
	    $state(messages).log configure -state normal
	    $state(messages).log insert end [lindex $args 0]
	    $state(messages).log configure -state disabled
	    $state(messages).log see end
	}
	addXMLError {
	    $state(messages).log configure -state normal

	    set xml [lindex $args 0]
	    set id 0
	    $state(messages).log insert end [mc "Problems detected in document:\n"]
	    foreach errormsg [lindex $args 1] {
		foreach {domain level code node line message relatedLine dummy related1 related2} $errormsg break
		lappend error($line) error$id
		lappend related($relatedLine) related$id
		$state(messages).log insert end $message [list error error$id]
		if {[string index $message end] != "\n"} {
		    $state(messages).log insert end \n
		}
		$state(messages).log tag bind error$id <Enter> [list ErrorHighlight $w $state(messages).log error$id $line $relatedLine]
		$state(messages).log tag bind error$id <Leave> [list ErrorRemoveHighlight $w $state(messages).log error$id $line $relatedLine]
		incr id
	    }
	    $state(messages).log insert end \n

	    set linenum 1
	    foreach line [split $xml \n] {

		if {[info exists error($linenum)]} {
		    $state(messages).log insert end $line "error errorline$linenum"
		} elseif {[info exists related($linenum)]} {
		    $state(messages).log insert end $line "related relatedline$linenum"
		} else {
		    $state(messages).log insert end $line
		}
		$state(messages).log insert end \n

		incr linenum
	    }

	    $state(messages).log configure -state disabled
	    $state(messages).log see end
	}
	addDocError {
	    $state(messages).log configure -state normal

	    set doc [lindex $args 0]
	    foreach errormsg [lindex $args 1] {
		foreach {domain level code node line message relatedLine dummy related1 related2} $errormsg break
		$state(messages).log insert end $message
		if {[string index $message end] != "\n"} {
		    $state(messages).log insert end \n
		}
	    }

	    $state(messages).log configure -state disabled
	    $state(messages).log see end
	}
	addMessage {
	    $state(messages).log configure -state normal
	    $state(messages).log insert end [lindex $args 0] message
	    $state(messages).log configure -state disabled
	    $state(messages).log see end
	}
	timing {
	    if {$state(timing)} {
		$state(messages).log configure -state normal
		$state(messages).log insert end [lindex $args 0] timing
		$state(messages).log configure -state disabled
		$state(messages).log see end
	    }
	}
	entity {
	    if {$state(display:entrefs)} {
		$state(messages).log configure -state normal
		$state(messages).log insert end [lindex $args 0] log \n
		$state(messages).log configure -state disabled
		$state(messages).log see end
	    }
	}
	default {
	    return -code error "unknown method \"$method\""
	}
    }

    return {}
}

# ErrorHighlight -- Highlight an error

proc ErrorHighlight {win log tag line related} {
    $log tag configure $tag -background [$log tag cget errorhighlight -background]
    $log tag configure errorline$line -background [$log tag cget errorhighlight -background]
    $log tag raise errorline$line error
    $log tag configure relatedline$related -background [$log tag cget relatedhighlight -background]
    $log tag raise relatedline$related related

    return {}
}
proc ErrorRemoveHighlight {win log tag line related} {
    Feedback $win {}

    $log tag configure $tag -background {}
    $log tag configure errorline$line -background {}
    $log tag configure relatedline$related -background {}

    return {}
}

# Feedback -- Manage the feedback widget

proc Feedback {win msg} {
    upvar \#0 State$win state

    set state(feedback) $msg
    update

    return {}
}
proc FeedbackProgress {win percent} {
    upvar \#0 State$win state

    $state(progress) coords $state(progressbar) 0 0 $percent 25
    update

    return {}
}

# Incr -- utility to increment a variable, handling non-existance

proc Incr var {
    upvar $var v
    if {[info exists v]} {
	incr v
    } else {
	set v 1
    }

    return $v
}

# This should be part of the uri package

proc uri_escape uri {
    # TODO: other characters must also be escaped
    regsub -all { } $uri {%20} uri

    return $uri
}


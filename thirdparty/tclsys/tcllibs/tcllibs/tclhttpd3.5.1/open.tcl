# opentrace.tcl --
#
# This is a wrapper around the open command that keeps track of what
# file (or pipe) was opened for a particular channel.  Useful for
# debugging file descriptor leaks.
#
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::opentrace 1.0

if {[info commands "open-orig"] == ""} {
    rename open open-orig
    proc open {path {mode r} {perms {}}} {
	global OpenFiles
	if {[catch {
	    if {[string length $perms]} {
		set io [open-orig $path $mode $perms]
	    } else {
		set io [open-orig $path $mode]
	    }
	} err]} {
	    return -code error $err
	}
	set OpenFiles($io) $path
	return $io
    }
}

if {[info commands "close-orig"] == ""} {
    rename close close-orig
    proc close {io} {
	global OpenFiles
	if {[info exist OpenFiles($io)]} {
	    set OpenFiles($io) "CLOSED $OpenFiles($io)"
	}
	close-orig $io
    }
}

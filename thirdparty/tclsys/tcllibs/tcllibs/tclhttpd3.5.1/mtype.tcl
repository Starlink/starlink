# mimetype.tcl --
# Code to deal with mime types
#
# Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::mtype 1.1

# Convert the file suffix into a mime type

# global MimeType is a mapping from file extension to mime-type

proc Mtype {path} {
    global MimeType

    set ext [string tolower [file extension $path]]
    if {[info exist MimeType($ext)]} {
	return $MimeType($ext)
    } else {
	return text/plain
    }
}

# Read a mime types file into the mimeType array.
# Set up some minimal defaults if no file is available.

proc Mtype_ReadTypes {file} {
    global MimeType

    array set MimeType {
	{}	text/plain
	.txt	text/plain
	.htm	text/html
	.html	text/html
	.tml	application/x-tcl-template
	.gif	image/gif
	.thtml	application/x-safetcl
	.shtml	application/x-server-include
	.cgi	application/x-cgi
	.map	application/x-imagemap
	.subst	application/x-tcl-subst
    }
    if [catch {open $file} in] {
	return
    }

    while {[gets $in line] >= 0} {
	if [regexp {^( 	)*$} $line] {
	    continue
	}
	if [regexp {^( 	)*#} $line] {
	    continue
	}
	if [regexp {([^ 	]+)[ 	]+(.+)$} $line match type rest] {
	    foreach item [split $rest] {
		if [string length $item] {
		    set MimeType([string tolower .$item]) $type
		}
	    }
	}
    }
    close $in
}

# Mtype_Accept --
#
#	This returns the Accept specification from the HTTP headers.
#	These are a list of MIME types that the browser favors.
#
# Arguments:
#	sock	The socket connection
#
# Results:
#	The Accept header, or a default.
#
# Side Effects:
#	None

proc Mtype_Accept {sock} {
    upvar #0 Httpd$sock data
    if {![info exist data(mime,accept)]} {
	return */*
    } else {
	return $data(mime,accept)
    }
}

# Mtype_Match --
#
# 	This compares a document type with the Accept values.
#
# Arguments:
#	accept	The results of Mtype_Accept
#	type	A MIME Content-Type.
#
# Results:
#	1	If the content-type matches the accept spec, 0 otherwise.
#
# Side Effects:
#	None

proc Mtype_Match {accept type} {
    foreach t [split $accept ,] {
	regsub {;.*} $t {} t	;# Nuke quality parameters
	set t [string trim [string tolower $t]]
	if {[string match $t $type]} {
	    return 1
	}
    }
    return 0
}

# Mtype_Add --
#
# 	Add a MIME type mapping
#
# Arguments:
#	suffix	A file suffix
#	type	The corresponding MIME Content-Type.
#
# Results:
#       None

proc Mtype_Add {suffix type} {
    global MimeType

    set suffix [string trimleft $suffix .]
    set MimeType([string tolower .$suffix]) $type
}

# Mtype_Reset --
#
# 	Clear all MIME type mappings
#
# Arguments:
#	None
#
# Side Effects:
#       Unsets the MimeType array

proc Mtype_Reset {} {
    global MimeType
    if {[info exist MimeType]} {
        unset MimeType
    }
}

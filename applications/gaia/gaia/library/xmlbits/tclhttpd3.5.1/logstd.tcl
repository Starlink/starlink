# logstd.tcl
#
# Standard Web Log file format support.
#
# Stephen Uhler / Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

# Use IP address, or domain name?
# Default is IP address, because looking up names is expensive
if {![info exists Log(lognames)]} {
    set Log(lognames) 0
}

# If true, we append cookie values to the log
if {![info exists Log(cookie)]} {
    set Log(cookie) 0
}

# LogStandardData --
#
#	Generate a standard web log file record for the current connection.
#	This records the client address, the URL, the time, the error
#	code, and so forth.
#
# Arguments:
#	sock	The client connection.
#	now	The timestamp for the connection, in seconds
#
# Results:
#	A string containing the log fie record.
#
# Side Effects:
#	None

proc LogStandardData {sock now} {
    return [LogStandardPrint [LogStandardList $sock $now]]
}

# LogStandardPrint --
#
#	Generate a standard web log file record for the current connection.
#	This records the client address, the URL, the time, the error
#	code, and so forth.
#
# Arguments:
#	sock	The client connection.
#	now	The timestamp for the connection, in seconds
#
# Results:
#	A string containing the log fie record.
#
# Side Effects:
#	None

proc LogStandardPrint {data} {
    set sep ""
    set result ""
    foreach {n v} $data {
	if {$v == "" || $v == "-"} {
	    append result ${sep}-
	    continue
	}
	switch -- $n {
	    time {
		 append result $sep\[[clock format $v -format "%d/%h/%Y:%T %Z"]\]
	    }
	    http -
	    referer -
	    useragent -
	    cookie {
		append result $sep"$v"
	    }
	    default {
		append result $sep$v
	    }
	}
	set sep " "
    }
    return $result
}

# LogStandardList --
#
#	Like LogStandardData, but return the data in a name, value list
#	suitable for use in foreach loops, array get, or long term
#	storage.
#
# Arguments:
#	sock	The client connection
#	now	The timestamp for the connection, in seconds
#
# Results:
#	A name, value list of the fields in a standard web log entry.
#
# Side Effects:
#	None

proc LogStandardList {sock now} {
    global Log
    upvar #0 Httpd$sock data
    if {$Log(lognames)} {
	if {[catch {lappend result ipaddr [Httpd_Peername $sock]}]} {
	    lappend result ipaddr [LogValue data(ipaddr)]
	}
    } else {
	lappend result	ipaddr [LogValue data(ipaddr)]
    }
    lappend result authuser [LogValue data(mime,auth-user)]
    lappend result username [LogValue data(mime,username)]
    lappend result time $now
    lappend result http [LogValue data(line)]
    lappend result status [LogValue data(code)]
    lappend result filesize [LogValue data(file_size)]
    lappend result referer [LogValue data(mime,referer)]
    lappend result useragent [LogValue data(mime,user-agent)]
    if {$Log(cookie)} {
      # This field is not always present in logs
      lappend result cookie [LogValue data(mime,cookie)]
    }
    return $result
}

# LogValue --
#
#	Generate a field or the default "null field" representation.
#
# Arguments:
#	var	The variable whose value to use, if any
#
# Results:
#	The value of the variable, or - as the default.
#
# Side Effects:
#	None

proc LogValue {var} {
    upvar $var data
    if {[info exists data]} {
	return $data
    } else {
       return -
    }
}

package provide httpd::logstd 1.0


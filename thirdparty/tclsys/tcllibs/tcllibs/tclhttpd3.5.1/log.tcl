# log.tcl
#
#	This is a file-based logging module for TclHttpd.
#
#	This starts a new log file each day with Log_SetFile
#	It also maintains an error log file that is always appeneded to
#	so it grows over time even when you restart the server.
#
# Stephen Uhler / Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::log 1.1
package require httpd::logstd 1.0

# log an Httpd transaction

# This program is used to compress log files
if {![info exists Log(compressProg)]} {
    set Log(compressProg) gzip
}

# Flush interval
if {![info exists Log(flushInterval)]} {
    set Log(flushInterval) [expr {60 * 1000}]
}

# This is used to turn on an alternate debug log file
if {![info exist Log(debug_log)]} {
    set Log(debug_log) 0
}


# Log --
#
#	Log information about the activity of TclHttpd.  There are two kinds of
#	log entries.  One "normal" entry that goes into its own log, one line
#	for each HTTP transaction.  All other log records are appended to an
#	error log file.
#
# Arguments:
#	sock	The client connection.
#	reason	If "Close", then this is the normal completion of a request.
#		Otherwise, this is some error tag and the record goes to
#		the error log.
#	args	Additional information to put into the logs.
#
# Results:
#	None
#
# Side Effects:
#	Writes data to the log files.

proc Log {sock reason args} {
    global Log
    upvar #0 Httpd$sock data

    # Log normal closes to the regular log.
    # Log everything else to the error log.

    switch -- $reason {
	"Close" {
	    set now [clock seconds]
	    set result [LogStandardData $sock $now]
	    if {[catch {puts $Log(log_fd) $result} err]} {
		set urk !
	    }
	    if {$Log(flushInterval) == 0} {
		catch {flush $Log(log_fd)}
	    }
	}
	"Debug" {
	    set now [clock seconds]
	    append result { } \[[clock format $now -format %d/%h/%Y:%T]\]
	    append result { } $sock { } $reason { } $args
	    if {[info exists data(url)]} {
		append result { } $data(url)
	    }
	    catch { puts $Log(debug_fd)  $result ; flush $Log(debug_fd) }
	}
	default {
	    set now [clock seconds]
	    append result { } \[[clock format $now -format %d/%h/%Y:%T]\]
	    append result { } $sock { } $reason { } $args
	    if {[info exists data(url)]} {
		append result { } $data(url)
	    }
	    catch { puts $Log(error_fd)  $result ; flush $Log(error_fd) }
	}
    }
}

# Log_Configure --
#
#	Query/modify configuration settings for logging.
#
# Arguments:
#	args	option/value pairs
#
# Results:
#	Configuration value(s) or empty string

proc Log_Configure args {
    global Log

    switch [llength $args] {
	0 {
	    foreach {key value} [array get Log] {
		lappend result [list -$key $value]
	    }
	    return $result
	}
	1 {
	    return $Log(-[lindex $args 0])
	}
	default {
	    if {[llength $args] % 2} {
		error "no value specified for option \"[lindex $args end]\""
	    } else {
		foreach {option value} $args {
		    switch -- $option {
			-lognames {
			    lappend newOptions lognames [boolean $value]
			}
			default {
			    # TODO: Other logging options, such as filenames, flush interval, etc
			    error "unknown option \"$option\""
			}
		    }
		}
		array set Log $newOptions
	    }
	}
    }
    return {}
}

# Log_FlushMinutes --
#
# Set the interval at which the logs are flushed.
#
# Arguments:
#	min	The minutes between flushes.  If 0,
#		then the log is flushed on each write.
#
# Results:
#	None
#
# Side Effects:
#	None

proc Log_FlushMinutes {min} {
    global Log
    set Log(flushInterval) [expr int($min*60*1000)]
    if {[info exist Log(flushID)]} {
	Log_Flush
    }
}

# Log_CompressProg --
#
# Set log compression program
#
# Arguments:
#	prog	the program used to compress logs
#
# Results:
#	None
#
# Side Effects:
#	None

proc Log_CompressProg {prog} {
    global Log
    set Log(compressProg) $prog
}

# Log_SetFile --
# automatically change log files every midnight
#
# Arguments:
#	basename 	The name of a base filename
#			including its directory,
#			e.g. /logs/www
#			This will create files like:
#			/logs/www80_00.09.23
#
# Results:
#	None
#
# Side Effects:
#	Closes and opens files, creating new files
#	each time through.

proc Log_SetFile {{basename {}}} {
    global Log
    if {[string length $basename]} {
	set Log(log) $basename
    }
    if {![info exists Log(log)]} {
	catch {close $Log(log_fd)}
	catch {close $Log(error_fd)}
	catch {close $Log(debug_fd)}
	return
    }
    catch {Counter_CheckPoint} 		;# Save counter data

    # set after event to switch files after midnight
    set now [clock seconds]
    set next [expr {([clock scan 23:59:59 -base $now] -$now + 1000) * 1000}]
    after cancel Log_SetFile
    after $next Log_SetFile

    # set the log file and error file.
    # Log files rotate, error files don't

    if {[info exists Log(log_file)] && [file exists $Log(log_file)]} {
	set lastlog $Log(log_file)
    }
    set Log(log_file) $Log(log)[clock format $now -format %y.%m.%d]
    catch {close $Log(log_fd)}

    # Create log directory, if neccesary, then open the log file

    catch {file mkdir [file dirname $Log(log_file)]}
    if {[catch {set Log(log_fd) [open $Log(log_file) a]} err]} {
	 Stderr $err
    }

    if {[info exists lastlog]} {
	# compress log files as we go
	if {[file executable $Log(compressProg)] &&
	    [catch {exec $Log(compressProg) $lastlog &} err]} {
	    Stderr $err
	}
    }

    catch {close $Log(error_fd)}
    if {[catch {set Log(error_fd) [open $Log(log)error a]} err]} {
	Stderr $err
    }

    # This debug log gets reset daily

    catch {close $Log(debug_fd)}
    if {[info exists Log(debug_file)] && [file exists $Log(debug_file)]} {
	catch {file rename -force $Log(debug_file) $Log(debug_file).old}
    }

    if {[info exist Log(debug_log)] && $Log(debug_log)} {
	set Log(debug_file) $Log(log)debug
	if {[catch {set Log(debug_fd) [open $Log(debug_file) w]} err]} {
	    Stderr $err
	}
    }
}

# Log_Flush --
# flush the output to the log file.  Do this periodically, rather than
# for every transaction, for better performance
#
# Arguments:
#
# Results:
#	None
#
# Side Effects:
#	Flushes the logs to disk.

proc Log_Flush {} {
    global Log
    catch {flush $Log(log_fd)}
    catch {flush $Log(error_fd)}
    catch {after cancel $Log(flushID)}
    if {$Log(flushInterval) > 0} {
	set Log(flushID) [after $Log(flushInterval) Log_Flush]
    }
}

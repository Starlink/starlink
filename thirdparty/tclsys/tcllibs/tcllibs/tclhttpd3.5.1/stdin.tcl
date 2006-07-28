# stdin.tcl
#
# (c) 1997 Sun Microsystems Laboratories (Stephen Uhler)
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

# The following is an event-driven command loop for testing with tclsh
# with command history:
#	!<pattern> run 1st command matching pattern
#	!<pattern>/xxx/yyy/ as above, but substitute yyy for xxx
# Nb: history doesn't work, too many changes to the tcl api to track.

package provide httpd::stdin 1.1

set Stdin(maxHistory) 100
append Stdin(history) {}
proc StdinRead {prompt} {
    global Stdin
    if [eof stdin] {
	set Stdin(Wait) 1
	fileevent stdin readable {}
	return
    }
    append Stdin(command) [gets stdin]
    if {[info complete $Stdin(command)]} {
	set s1 ""; set s2 ""
	if {[regexp {^!([^/]+)(/([^/]+)/([^/]*))?} $Stdin(command) \
		    {} history {} s1 s2]} {
	    set match [lsearch -regexp $Stdin(history) "^ *$history"]
	    if {$match >= 0} {
	    	set Stdin(command) [lindex $Stdin(history) $match]
		if {$s1 != ""} {
		    catch {regsub -- $s1 $Stdin(command) $s2 Stdin(command)}
		}
		puts stderr $Stdin(command)
	    } else {
	    	puts -nonewline "No \"$history\" in history list\n$prompt"
		set Stdin(command) ""
		flush stdout
		return
	    }
	}
	if {$Stdin(command) != "" && \
		$Stdin(command) != [lindex $Stdin(history) 0]} {
	    set Stdin(history) [lrange "[list $Stdin(command)] \
		    $Stdin(history)" 0 $Stdin(maxHistory)]
	}
        catch {uplevel #0  $Stdin(command)} result
        puts -nonewline "$result\n$prompt"
        flush stdout
        set Stdin(command) ""
    } else {
    	append Stdin(command) \n
    }
}

proc Stdin_Start {{prompt "% "}} {
    global Stdin
    set Stdin(command) ""
    puts -nonewline $prompt
    flush stdout
    fileevent stdin readable [list StdinRead $prompt]
    vwait Stdin(wait)
}

if {[info procs bgerror] == ""} {
    proc bgerror {args} {
	global errorInfo
	Log {} bgerror $errorInfo ; Log_Flush
	puts stderr $errorInfo
    }
}					

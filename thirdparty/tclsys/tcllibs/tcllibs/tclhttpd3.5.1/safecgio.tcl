# safecgio.tcl
# Brent Welch (c) 1996 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
#
# RCS: @(#) $Id$
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

# Safe Tcl security policy for server-side CGI-like processing.
# The HTTPD will "subst" an html page in the context of a safecgio interpreter
# in order to efficiently compute a page.  This security policy gives the
# safe interpreter limited access to files within a specified directory.
#
#  The script has access to the "env" array, which contains the same
#  information as for a CGI bin script.
#
#  Any query information, either from a POST or GET query is available
#  as a name-value list in the "Query" variable.

package provide httpd::safecgio 1.0

# start a safe CGI server 

proc SafeCGI_Server {port} {
    if {[catch {socket -server SafeCGI_Accept $port} oops]} {
	Stderr "SafeCGI_Server: $oops"
	exit 1
    }
}

# Accept a connection from Httpd server.

proc SafeCGI_Accept {sock addr port} {
    fconfigure $sock -blocking off
    fileevent $sock readable [list SafeCGIRead $sock]
}

# Read and execute commands from the Http server

proc SafeCGIRead {sock} {
    global Env Query

    if {[eof $sock]} {
	close $sock
    } else {
	set command {}
	foreach {command value} [gets $sock] break
	switch -- $command {
	    SetEnv	{ catch {array set Env $value} }
	    SetQuery	{ set Query $value }
	    DoFile	{ SafeCGIDo $sock $value }
	    Exit	{ exit }
	}
    }
}

# Create a safe interpreter to "subst" a data file.

proc SafeCGIDo {sock filename} {
    global Query Env argv0

    if {![catch {open $filename} fd]} {
	append Query {}
	set child child$sock		;# Name of child interpreter
	SafeCGI_Create $child 
	interp transfer {} $fd $child
	interp eval $child {
	    proc DoCGI {fd Query envlist} {
		global env
		array set env $envlist
		subst [read $fd]
	    }
	}

	if {![catch {
	    interp eval $child [list DoCGI $fd $Query [array get Env]]
	} result]} {
	    puts -nonewline $sock $result
	} else {
	    Log $sock SafeCGIDo "$argv0 Error in $filename\n\t<$result>"
	}
	interp delete $child		;# Does close of transferred fd
    }
    close $sock
}

# security policy defined below.
#####################################################################
#
# Create a child that is limted to a few open files within
# a given directory.  If they open for writing, limit the size
# of the output file.

proc SafeCGI_Create {name {directory .} {maxopen 4} {maxsize 4096}} {
    global interpState
    interp create -safe $name
    interp alias $name open {} SafeCGI_Open $name $maxopen $directory
    interp eval  $name {rename puts {}}
    interp alias $name puts {} SafeCGI_Puts $name $maxsize
    interp eval  $name {rename close _close}	;# would rather "hide" this
    interp alias $name close {} SafeCGI_Close $name
    interp alias $name exit {} SafeCGI_Cleanup $name
    set interpState(channels,$name) {}
}

# Open files in a given directory.
# Paramters set by the definition of the alias:
# 	interp is a child interperter
# 	maxopen is the max number of open files for the child
# 	directory is the directory
# Parameters from the child's call
# 	file is the name of the file
# 	access is the I/O mode

proc SafeCGI_Open {interp maxopen directory file {access r}} {
    global interpState

    if {[llength $interpState(channels,$interp)] >= $maxopen} {
	error "couldn't open \"$file\": too many open files"
    }
    if [regexp "^\[ \t\]*|" $file] {
	error "no pipes allowed"
    }
    # Constrain the pathname to a one-component relative name
    if {([llength [file split $file]] != 1) ||
	    [file pathtype $file] != "relative"} {
	error "couldn't open \"$file\": permission denied"
    }
    # Disallow symbolic links and other non-file types
    set path [file join $directory $file]
    if [file exists $path] {
	file lstat $path stat		;# lstat here prevents symlinks
	if {$stat(type) != "file"} {
	    error "couldn't open \"$file\": not a plain file"
	}
    }
    set out [open $path $access]
    lappend interpState(channels,$interp) $out
    interp share {} $out $interp
    return $out
}
    # This is an alias to clean up children that exit
    # interp is the exiting child
    # interpState records open files for the child
    # We share them, so we must close our references.

proc SafeCGI_Cleanup {interp} {
    global interpState

    interp delete $interp
    foreach out $interpState(channels,$interp) {
	catch {close $out}
    }
    unset interpState(channels,$interp)
}

# This is an alias for puts that limits file size
# Parameters set by the alias definition:
# 	interp is the child interpreter
# 	max is the file size limit, in bytes
# Parameters set in the call to the alias
#   args is ?-nonewline? chan string
# 		chan is the I/O channel
# 		string is the output data

proc SafeCGI_Puts {interp max args} {
    if {[string match "-nonewline" [lindex $args 0]]} {
	set flag -nonewline
	set args [lrange $args 1 end]
    } else {
	set flag ""
    }
    if {[llength $args] == 1} {
	# Flag output to stdout as coming from the unsafe interpreter
	set chan stdout
	set string "Untrusted code $name says: [lindex $args 0]"
    } elseif {[llength $args] == 2} {
	set chan [lindex $args 0]
	set string [lindex $args 1]
    } else {
	error "wrong # args: should be \"puts ?-nonewline? ?channelId? string\""
    }
    if {[string match "stdout" $chan"]} {
	set size [string length $string]	;# Tell makes no sense
    } else {
	set size [expr [tell $chan] + [string length $string]]
    }
    if {$size > $max} {
	    interp eval $interp [list close $chan]
	    error "File size exceeded"
    } else {
	    eval puts $flag {$chan $string}
    }
}

# An alias for close that lets the parent clean up its reference

proc SafeCGI_Close {interp chan} {
	global interpState
	set ix [lsearch $interpState(channels,$interp) $chan]
	if {$ix >= 0} {
	    catch {close $chan}
	    set interpState(channels,$interp) \
		    [lreplace $interpState(channels,$interp) $ix $ix]
	}
	$interp eval _close $chan	;# Need "invoke" hidden close
}

    proc bgerror {msg} {
	global errorInfo
	Stderr $errorInfo
    }

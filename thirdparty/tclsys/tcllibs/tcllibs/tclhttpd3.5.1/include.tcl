# include.tcl
#
# Stephen Uhler (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

# Process server side includes.
# Look for comments of the form:
#   <!--#keyword args  -->
# Where "keyword" is one of:
#  ECHO FLASTMOD FSIZE INCLUDE EXEC
# We do not implement
#  CONFIG
# The input string is passed on the command line to Httpd_include,
# and the result is returned.
# Note: the included file is treated as text, which possibly has
# other include files in it.  We are not doing a full URL dispatch
# on the included files, so they cannot be cgi scripts or other fancy
# objects.

package provide httpd::include 1.0
package require html

# Global state for this module
# maxdepth controls recursively included files.

array set Include {
	maxdepth 10
}

# Process a file with server includes

proc Doc_application/x-server-include {path suffix sock} {
    Count includes
    if {[catch {open $path} in]} {
	Httpd_Error $sock 404 $in
    } else {
	global env
	Cgi_SetEnv $sock $path 
	set html [Include_Html $sock $path [read $in]]
	close $in
	Httpd_ReturnData $sock text/html $html
    }
}

# Process all the server includes.
# sock: a token passed through to the include procedures
# path: required to handle relative paths in recursive includes
# html:  The html to process
# depth: A counter to detect include loops

proc Include_Html {sock path html {depth 0}} {
    set token \x87	;# this character never appears in an html stream
    regsub -all {([][$\\])} $html {\\\1} html	;# protect TCL special characters
    regsub -all -- --> $html $token html	;# convert end of comment to token
    regsub -all "<!--# *(\[^ ]+) *(\[^$token]+)$token" $html \
	    "\[IncludeInner $sock {$path} {\\1} {\\2} [incr depth]\]" html	;# find all includes
    regsub -all $token $html --> html		;# recover end of comment tokens
    return  [subst $html]			;# process the includes
}

proc IncludeInner {sock path command params depth} {
    set command [string tolower $command]
    if {![iscommand include_$command]} {
	return "<!-- Server not configured to processes \"$command\" includes -->\n"
    } elseif {[catch {include_$command $sock $path $params $depth} result]} {
    	return "<!-- Server error in include command $command: $result -->\n"
    } else {
    	return $result
    }
}

# utility to extract the file name specified from the include parameters

proc IncludeFile {sock op path params} {
    if {[html::extractParam $params virtual orig]} {
	set key virtual
	set npath [Doc_Virtual $sock $path $orig]
    } elseif {[html::extractParam $params file orig]} {
	set key file
	set npath [file join [file dirname $path] $orig]
    } else {
	error "<!-- Invalid $op parameter list: $params. -->\n"
    }
    return [list $key $npath $orig]
}

# Each server function has its own procedure, that returns the substituted value
###############################################################################

# include another file
# Params:
#  virtual=path
#  file=path

proc include_include {sock path params depth} {
    global Include
    if {$depth > $Include(maxdepth)} {
    	return "<!-- Include recursion depth exceeded ($depth) -->\n"
    }
    if {[catch {IncludeFile $sock include $path $params} info]} {
	return $info
    }
    set key [lindex $info 0]
    set npath [lindex $info 1]
    set orig [lindex $info 2]

    # now open the file and re-do substitutions.

    if {[catch {open $npath r} fd]} {
	return "<!-- invalid include $key path $orig: $fd -->\n"
    }
    set data [Include_Html $sock $npath [read $fd] $depth]
    close $fd
    return $data
}

proc include_echo {sock path params args} {
    global env
    set var ""
    if {[html::extractParam $params var]} {
	if {[info exists env($var)]} {
	    return $env($var)
	} else {
	    return "<!-- No such variable: $var. -->\n"
	}
    }
    return "<!-- Echo: No var parameter. -->\n"
}

proc include_fsize {sock path params args} {
    if [catch {IncludeFile $sock fsize $path $params} info] {
	return $info
    }
    set key [lindex $info 0]
    set npath [lindex $info 1]
    set orig [lindex $info 2]
    if {[file exists $npath]} {
	return [file size $npath]
    } else {
	return "<!-- File not found $key: $npath. -->\n"
    }
}

proc include_exec {sock path params args} {
    set cmd ""
    if {[html::extractParam $params cmd]} {
	if {[catch {eval exec $cmd} result]} {
	    regsub -all -- --> $result {} result
	    return "<!-- $cmd error: $result  -->\n"
	} else {
	    return $result
	}
    }
}

proc include_config {sock path params args} {
    return "<!-- include config not implemented -->\n"
}

proc include_flastmod {sock path params args} {
    if {[catch {IncludeFile $sock flastmod $path $params} info]} {
	return $info
    }
    set key [lindex $info 0]
    set npath [lindex $info 1]
    set orig [lindex $info 2]
    if {[file exists $npath]} {
	return [clock format [file mtime $npath]]
    } else {
	return "<!-- File not found $key: $npath. -->\n"
    }
}

# cgi.tcl
# CGI support
# Stephen Uhler / Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::cgi 1.1

# Cgi parameters
# timeout	Seconds before pipe to CGI program is closed
# maxcgi	Number of concurrent requests allowed
# cgi		Running total of cgi requests
# ident		If true, try to use the ident protocol
# env-pass	List of environment variables to preserve when setting
#		up the environment.  The rest are deleted and then recreated
#		from information in the CGI request.
# tclsh		Pathname to a Tcl interpreter.  If you preserve the env(PATH)
#		by listing PATH in env-pass, this can be "tclsh80.exe"
#		or "protclsh80.exe".  Otherwise it has to be an absolute path.

array set Cgi {
    timeout	300000
    maxcgi	100
    cgi		0
    ident	0
    env-pass	{PATH LD_LIBRARY_PATH TZ}
}
if {"$tcl_platform(platform)" == "windows"} {

    # On windows we hard-code the interpreters for various script types

    regsub -nocase wish [info nameofexecutable] tclsh Cgi(tclsh) ;# For .tcl
    set Cgi(perl) "perl.exe"			;# For .pl
    set Cgi(cgiexe) $Cgi(tclsh)			;# For .cgi

    # More environment variables need to be passed through.
    # Windows NT Need Lib and SystemRoot so DLLs loaded by tcl are found.
    # Windows 95 needs more... so we just pass everything

    lappend Cgi(env-pass) * 
}

# Register a cgi-bin directory.
# This causes the server to pay attention to the extra path information
# after the program name.

proc Cgi_Directory {virtual {directory {}}} {

    # Set up the URL-directory mapping so that DocAccessHook works right.

    if {[string length $directory] == 0} {
	set directory [Doc_Virtual {} {} $virtual]
    }
    Doc_RegisterRoot $virtual $directory

    # Register the domain - not in a thread becaused we'll use another process anyway.
    # The CGI module will also read the post data itself so TclHttpd doesn't buffer
    # it all in memory before passing it to the CGI process.

    Url_PrefixInstall $virtual [list Cgi_Domain $virtual $directory] \
	-thread 0 -readpost 0
}

# Cgi_Domain is called from Url_Dispatch for URLs inside cgi-bin directories.

proc Cgi_Domain {virtual directory sock suffix} {
    global Cgi env

    # Check the path and then find the part beyond the program name.
    # The trimleft avoids a buildup of extra / after the domain prefix.

    if [catch {Url_PathCheck [string trimleft $suffix /]} pathlist] {
	Doc_NotFound $sock
	return
    }

    # url is the logical name as viewed by a browser
    # path is a physical file system name as viewd by the server

    set url [string trimright $virtual /]
    set path $directory
    set i 0
    foreach component $pathlist {
	incr i
	set path [file join $path $component]
	append url /$component
	if {[file isfile $path]} {
#	    if {[file executable $path]} {
		# Don't bother testing execute permission here,
		# because it doens't test right on windows anyway.
		set extra [lrange $pathlist $i end]
#	    }
	    break
	} elseif {![file exists $path]} { 
	    Doc_NotFound $sock
	    return
	}
    }
    if {![info exists extra]} {
	# Didn't find an executable file
	Httpd_Error $sock 403
	return
    } elseif {[llength $extra]} {
	set extra /[join $extra /]
    }

    # The CGI needs the server-relative url of the script,
    # the extra part after the program name,
    # and the translated version of the whole pathname.

    Url_Handle [list CgiHandle $url $extra $path] $sock
}

proc CgiHandle {url extra path sock} {
    global Doc env
    Cgi_SetEnvAll $sock $path $extra $url env
    CgiSpawn $sock $path
}

# This gets called if there is no extra pathname after
# the name of a cgi script, and the Doc module finds a .cgi file

proc Doc_application/x-cgi {path suffix sock} {
    upvar #0 Httpd$sock data
    Url_Handle [list CgiHandle $data(url) {} $path] $sock
}

# Set the environment for the cgi scripts.  This is passed implicitly to
# the cgi script during the "open" call.

proc Cgi_SetEnv {sock path {var env}} {
    upvar 1 $var env
    upvar #0 Httpd$sock data
    Cgi_SetEnvAll $sock $path {} $data(url) env
}

proc Cgi_SetEnvInterp {sock path interp} {
    upvar #0 Httpd$sock data
    Cgi_SetEnvAll $sock $path {} $data(url) env
    interp eval $interp [list uplevel #0 \
	[list array set env [array get env]]]
}

proc Cgi_SetEnvAll {sock path extra url var} {
    upvar #0 Httpd$sock data
    upvar 1 $var env
    global Httpd Httpd_EnvMap Cgi

    # we can't "unset env" or it won't get passed to the CGI script
    foreach i [array names env] {
	set clear 1
	foreach x $Cgi(env-pass) {
	    if {[string match $x $i]} {
		# Preserve some path settings
		set clear 0
		break
	    }
	}
	if {$clear} {
	    unset env($i)
	}
    }
    foreach name [array names Httpd_EnvMap] {
	set env($name) ""
	catch {
	    set env($name) $data($Httpd_EnvMap($name))
	}
    }
    set env(REQUEST_URI) [Httpd_SelfUrl $data(uri) $sock]
    set env(GATEWAY_INTERFACE) "CGI/1.1"
    set env(SERVER_PORT) [Httpd_Port $sock]
    if {[info exist Httpd(https_port)]} {
	set env(SERVER_HTTPS_PORT) $Httpd(https_port)
    }
    set env(SERVER_NAME) $Httpd(name)
    set env(SERVER_SOFTWARE) $Httpd(server)
    set env(SERVER_PROTOCOL) HTTP/1.0
    set env(REMOTE_ADDR) $data(ipaddr)
    set env(SCRIPT_NAME) $url
    set env(PATH_INFO) $extra
    set env(PATH_TRANSLATED) [string trimright [Doc_Root] /]/[string trimleft $data(url) /]
    set env(DOCUMENT_ROOT) [Doc_Root]
    set env(HOME) [Doc_Root]

    if {$Cgi(ident)} {
	catch {
	    set ident [::ident::get $sock]
	    set env(REMOTE_IDENT) [lindex $ident 0]
	}
    }

    if {$data(proto) == "POST"} {
	set env(QUERY_STRING) ""
    }
}

# Enable/disable IDENT lookup.  Disabled by default.

proc Cgi_Ident state {
    global Cgi

    regsub {^(1|yes|true|on)$} $state 1 state
    regsub {^(0|no|false|off)$} $state 0 state

    if {$state} {
	package require ident
    }

    set Cgi(ident) $state
}

# Run a cgi script:
# Our caller needs to set up the environment with Cgi_SetEnv.
# Run the script, and copy the
# output to the socket. 
# Set a timer to deal with invalid scripts.

proc CgiSpawn {sock script} {
    upvar #0 Httpd$sock data
    global env Cgi

    if {$Cgi(cgi) >= $Cgi(maxcgi)} {
	Httpd_Error $sock 504  "Too many CGI's"
	return
    }
    incr Cgi(cgi)

    # for GET queries, pass the query as an argument to the cgi script
    if {$data(proto) == "POST"} {
	set arglist ""
    } else {
	set arglist $data(query)
    }
    set pwd [pwd]
    cd [file dirname $script]
    if {[catch {CgiExec $script $arglist} fd]} {
	cd $pwd
	Httpd_Error $sock 400 $fd
	incr Cgi(cgi) -1
	return
    }
    cd $pwd
    Count cgihits
    set data(infile) $fd	;# So close happens in Httpd_SockClose
    set data(header) 0		;# Have not read header yet
    set data(headerlist) {}	;# list of read headers
    set data(headercode) "200 data follows"	;# normal return
    fconfigure $fd -blocking 0

    # Set up a timer in case it hangs

    catch {after cancel $data(cancel)}
    set data(cancel) [after $Cgi(timeout) CgiCancel $fd $sock]

    if {$data(proto) == "POST"} {

        # Read the pipe in auto mode to properly detect the end of
        # headers in Unix and Windows.  Pump the POST data in
        # binary mode.

	fconfigure $fd -translation {auto binary}

	if {$data(count) == 0} {

	    # Either there was no POST data, or we are inside a domain
	    # that automatically read the POST data into data(query) already
	    # Errors appear here because of the non-blocking writes.

	    catch {
		puts -nonewline $fd $data(query)
		flush $fd
	    }
	} else {

	    # Pump the query data to the CGI process in the background
	    # We set up fileevents after this finishes, so return now

	    # fcopy bug in Tcl 8.3.2 and Tcl 8.4a2 and all previous versions 
	    # prevents this from working reliably on large amounts of POST data
	    # fcopy $sock $fd -command [list CgiCopyDone $sock $fd] -size $data(count)
	    fileevent $sock readable [list CgiCopyPost $sock $fd]
	    return
	}
    }

    CgiCopyDone $sock $fd $data(count) ""
}

# CgiCopyPost --
#
#	Called to copy POST data down the pipe - need to avoid fcopy bug

proc CgiCopyPost {sock fd} {
    upvar #0 Httpd$sock data
    global tcl_version

    set buf [read $sock $data(count)]
    puts -nonewline $fd $buf
    if {$tcl_version <= 8.0} {
	set len [string length $buf]
    } else {
	# Ensure we get byte count, not I18N character count
	set len [string bytelength $buf]
    }
    set data(count) [expr {$data(count) - $len}]
    if {$data(count) <= 0} {
	flush $fd
	CgiCopyDone $sock $fd dummy ""
    }
}

# CgiCopyDone --
#
#	Called when we have finished copying POST data to the CGI process

proc CgiCopyDone {sock fd bytes {errmsg {}}} {
    upvar #0 Httpd$sock data

    # Reset this otherwise the Httpd module tries to read the
    # extra data

    set data(count) 0

    if {[string length $errmsg]} {
	CgiCancel
    } else {
	# Wait for output from the CGI process (CgiRead)
	# Watch the socket (CgiCleanup)
	# In a worker thread, this is not really a socket, hence the catch.

	catch {fileevent $sock readable [list CgiCleanup $fd $sock]}
	fileevent $fd readable [list CgiRead $fd $sock]
    }
}

# Execute the CGI process.

proc CgiExec {script arglist} {
    global tcl_platform
    global Cgi
    switch -- $tcl_platform(platform) {
	unix {
	    if {[file exists /dev/stdout]} {

		# This trick "2> /dev/stdout" fails if you start the process in
		# the background under an xterm, then quit the xterm.  In that case
		# the open raises an error.

		if { ! [catch {open "|[list $script] $arglist 2> /dev/stdout" r+} pipe]} {
		    return $pipe
		}
	    }

	    # We use cat to merge the stderr and stdout

	    return [open "|[list $script] $arglist |& cat" r+]
	}
	windows {
	    switch -- [file extension $script] {
		.pl {
		    return [open "|[list $Cgi(perl) $script] $arglist" r+]
		}
		.cgi {
		    return [open "|[list $Cgi(cgiexe) $script] $arglist" r+]
		}
		.tcl {
		    return [open "|[list $Cgi(tclsh) $script] $arglist" r+]
		}
		.exe {
		    return [open "|[list $script] $arglist" r+]
		}
		default {
		    error "Don't know how to execute CGI $script"
		}
	    }
	}
	macintosh {
	    error "CGI not supported on Macintosh"
	}
    }
}

# Read data from the CGI script, write to client.

proc CgiRead {fd sock} {
    upvar #0 Httpd$sock data
    global Cgi Httpd

    if {[eof $fd]} {
	CgiClose $fd $sock
    } elseif {[catch {
	if {!$data(header)} {

	    # Read and accumulate headers until we know what kind
	    # of response to make

	    set n [gets $fd line]
	    if {$n < 0} {
		# Blocked or EOF - do nothing.
		# EOF will be caught on next fileevent.
		# Blocked means we have a partial line, just wait.
	    } elseif {$n == 0} {
		set data(header) 1
		append header "HTTP/1.0 $data(headercode)\n"
		append header "Server: $Httpd(server)\n"
		append header "Date: [HttpdDate [clock seconds]]\n"
		append header "Connection: Close\n"
		foreach line $data(headerlist) {
		    append header $line\n
		}
		append header "\n"
		CgiPutHeader $sock $header
	    } else {
		lappend data(headerlist) $line
		if {[regexp -nocase ^(location|uri): $line]} {
		    set data(headercode) "302 found"
		}
	    }
	} else {

	    CgiCopy $fd $sock
	}
    } oops]} {
	# Socket may have gone away
	CgiCancel $fd $sock
    }
}

# Emit the header, handling the case where we are in a worker thread.

proc CgiPutHeader {sock header} {
    upvar #0 Httpd$sock data
    if {[info exist data(master_thread)] &&
            $data(master_thread) != [Thread_Id]} {

        Thread_Send $data(master_thread) \
            [list CgiCopyDirect $sock $header]
        Thread_Send $data(master_thread) \
            [list fconfigure $sock -translation binary]
    } else {
        puts -nonewline $sock $header
    }

}

# Copy data from the pipe to the socket

proc CgiCopy {fd sock} {
    upvar #0 Httpd$sock data
    global Httpd

    fconfigure $fd -translation binary
    if {[info exist data(master_thread)] &&
            $data(master_thread) != [Thread_Id]} {
        # Read the data and copy it to the main thread for return.
        # Quick and dirty until we can transfer file descriptors.
        # The pipe is already non-blocking - read what we can

        Thread_SendAsync $data(master_thread) \
                    [list CgiCopyDirect $sock [read $fd]]

        return
    }

    # Normal one-thread case - set up to copy data from pipe to socket

    fconfigure $sock -translation binary
    fileevent $sock readable {}
    fileevent $fd readable {}
    fcopy $fd $sock -command [list CgiClose $fd $sock]
}

proc CgiCopyDirect {sock block} {
    puts -nonewline $sock $block
    flush $sock
}

# This is installed after running the sub-process to
# check for eof on the socket before the processing is complete.

proc CgiCleanup {fd sock} {
    fconfigure $sock -blocking 0 -translation auto
    set n [gets $sock line]
    if [eof $sock] {
	CgiCancel $fd $sock
    }
}

# Cancel a cgi script if it is taking too long.

proc CgiCancel {fd sock} {
    upvar #0 Httpd$sock data

    if {[info exist data(url)]} {
	Log $sock CgiCancel $data(url)
    }
    catch {exec kill [pid $fd]}
    CgiClose $fd $sock
}

# Close down the CGI connection

proc CgiClose {fd sock {bytes {}} {error {}}} {
    global Cgi
    upvar #0 Httpd$sock data

    catch {after cancel $data(cancel)}
    incr Cgi(cgi) -1
    if {![info exists data(header)]} {
	Httpd_Error $sock 204
    } else {
	Httpd_SockClose $sock 1
    }
    if {[string length $error] > 0} {
	Log $sock CgiClose $error
    }
}

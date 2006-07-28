# httpd.tcl --
#
# HTTP 1.0 protocol stack, plus connection keep-alive and 1.1 subset.
# This accepts connections and calls out to Url_Dispatch once a request
# has been recieved.  There are several utilities for returning
# different errors, redirects, challenges, files, and data.
#
# For binary data transfer this uses unsupported0 or fcopy.
# Tcl8.0a2 was the last release with unsupported0.
# Note that Tcl8.0b1 has a bug in fcopy where if an error occurs then
# bgerror is called instead of the command callback to fcopy.  This
# causes file descriptor leaks, so don't use 8.0b1 for real servers.
#
# For async operation, such as long-lasting server-side operations use
# Httpd_Suspend.
#
# Copyright
# Matt Newman (c) 1999 Novadigm Inc.
# Stephen Uhler / Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# Brent Welch (c) 2001-2004 Panasas Inc
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd 1.7

# initialize all the global data

# Location of this package
set Httpd(library) [file dirname [info script]]

# HTTP/1.0 error codes (the ones we use)
array set Httpd_Errors {
    200 {Data follows}
    204 {No Content}
    302 {Found}
    304 {Not Modified}
    400 {Bad Request}
    401 {Authorization Required}
    403 {Permission denied}
    404 {Not Found}
    408 {Request Timeout}
    411 {Length Required}
    419 {Expectation Failed}
    500 {Server Internal Error}
    501 {Server Busy}
    503 {Service Unavailable}
    504 {Service Temporarily Unavailable}
}
# Environment variables that are extracted from the mime header
# by Cgi_SetEnv.  The values are keys into the
# per-connection state array (i.e. "data")

array set Httpd_EnvMap {
    CONTENT_LENGTH	mime,content-length
    CONTENT_TYPE	mime,content-type
    HTTP_ACCEPT		mime,accept
    HTTP_AUTHORIZATION	mime,authorization
    HTTP_FROM		mime,from
    HTTP_REFERER	mime,referer
    HTTP_USER_AGENT	mime,user-agent
    QUERY_STRING	query
    REQUEST_METHOD	proto
    HTTP_COOKIE         mime,cookie
    HTTP_FORWARDED      mime,forwarded
    HTTP_HOST           mime,host
    HTTP_PROXY_CONNECTION mime,proxy-connection
    REMOTE_USER		remote_user
    AUTH_TYPE		auth_type
}

# The per-connection state is kept in the "data" array, which is
# really an array named Httpd$sock - (note the upvar #0 trick throughout)
# The elements of this array are documented here.  URL implementations
# are free to hang additional state off the data array so long as they
# do not clobber the elements documented here:

# These fields are semi-public, or "well known".  There are a few
# API's to access them, but URL implementations can rely on these:
#
# self		A list of protocol (http or https), name, and port that
#		capture the server-side of the socket address
#		Available with  Httpd_Protocol, Httpd_Name, and Httpd_Port API.
# uri		The complete URL, including proto, servername, and query
# proto		http or https
# url		The URL after the server name and before the ?
# query		The URL after the ?
# ipaddr	The remote client's IP address
# cert		Client certificate (The result of tls::status)
# host		The host specified in the URL, if any (proxy case)
# port		The port specified in the URL, if any
# mime,*	HTTP header request lines (e.g., mime,content-type)
# count		Content-Length
# set-cookie	List of Set-Cookie headers to stick into the response
#		Use Httpd_SetCookie to append to this.
# headers		List of http headers to stick into the response
#		Use Httpd_AddHeaders to append to this.

# prefix	(Set by Url_Dispatch to be the URL domain prefix)
# suffix	(Set by Url_Dispatch to be the URL domain suffix)

# auth_type	(Set by the auth.tcl module to "Basic", etc.)
# remote_user	(Set by the auth.tcl to username from Basic authentication)
# session	(Set by the auth.tcl to "realm,$username" from Basic auth)
#		You can overwrite this session ID with something more useful

# Internal fields used by this module.
# left		The number of keep-alive connections allowed
# cancel	AfterID of event that will terminate the connection on timeout
# state		State of request processing
# version	1.0 or 1.1
# line		The current line of the HTTP request
# mimeorder	List indicating order of MIME header lines
# key		Current header key
# checkNewLine	State bit for Netscape SSL newline bug hack
# callback	Command to invoke when request has completed
# file_size	Size of file returned by ReturnFile
# infile	Open file used by fcopy to return a file, or CGI pipe

# Httpd_Init
#	Initialize the httpd module.  Call this early, before Httpd_Server.
#
# Arguments:
#	none
#
# Side Effects:
#	Initialize the global Httpd array.
# bufsize:	Chunk size for copies
# initialized:	True after server started.
# ipaddr:	Non-default ipaddr for the server (for multiple interfaces)
# library:	a directory containing the tcl scripts.
# port:		The port this server is serving
# listen:	the main listening socket id
# server:	The server ID for the HTTP protocol.
# shutdown:	A list of Tcl callbacks made when the server shuts down
# sockblock:	blocking mode value for sockets (normally this should be 0)
# timeout1:	Time before the server closes a kept-alive socket (msecs)
# timeout2:	Time before the server kills an in-progress transaction.  (msecs)
# timeout3:	Time allowed to drain extra post data
# version:	The version number.
# maxused:	Max number of transactions per socket (keep alive)

proc Httpd_Init {} {
    global Httpd
    array set Httpd {
	timeout1	120000
	timeout2	120000
	timeout3	2000
	server		"Tcl-Webserver/"
	initialized 	1
	shutdown	""
	sockblock	0
	bufsize		16384
	maxused		25
    }
    if {![info exist Httpd(maxthreads)]} {
	set Httpd(maxthreads) 0
    }
    Httpd_Version
    append Httpd(server) $Httpd(version)
}

# Httpd_Server --
#	Start the server by listening for connections on the desired port.
#	This may be re-run to re-start the server.  Call this late,
# 	fter Httpd_Init and the init calls for the other modules.
#
# Arguments:
#	port	The TCP listening port number
#	name	The qualified host name returned in the Host field.  Defaults
#		to [info hostname]
#	ipaddr	Non-default interface address.  Otherwise IP_ADDR_ANY is used
#		so the server can accept connections from any interface.
#
# Results:
#	none
#
# Side Effects:
#	This sets up a callback to HttpdAccept for new connections.

proc Httpd_Server {{port 80} {name {}} {ipaddr {}}} {
    global Httpd

    if {![info exists Httpd(initialized)]} {
	Httpd_Init
    }
    catch {close $Httpd(listen)}
    set Httpd(name) $name
    set Httpd(ipaddr) $ipaddr
    set Httpd(port) $port
    if {[string length $name] == 0} {
	set Httpd(name) [info hostname]
    }
    set cmd [list socket -server [list HttpdAccept \
	    [list http $name $port]]]
    if {[string length $ipaddr] != 0} {
        lappend cmd -myaddr $ipaddr
    }
    lappend cmd $port
    if {[catch $cmd Httpd(listen)]} {
        return -code error "$Httpd(name):$port $Httpd(listen)\ncmd=$cmd"
    }
}

# Httpd_ServerShutdown --
#
#	Close the server's HTTP socket.
#
# Arguments:
#	none
#
# Results:
#	Returns "" if the socket was successfully closed, otherwise an error string.
#
# Side Effects:
#	Close the server's HTTP listening socket.

proc Httpd_ServerShutdown {} {
    global Httpd
    Log {} ShutdownSocket
    catch {close $Httpd(listen)} err
    return $err
}

proc Httpd_VirtualHost {host file} {
    return [Httpd_VirtualHosts [list $host] $file]
}

proc Httpd_VirtualHosts {hostNames file} {
    variable virtual

    foreach host $hostNames {
        set host [string tolower $host]
        if {[info exists virtual($host)]} {
	    error "Virtual host $host already exists"
        }
    }
    set slave [interp create]

    # Transfer the scalar global variables
    foreach var {::v ::auto_path} {
	$slave eval [list set $var [set $var]]
    }
    # Transfer the array global variables
    foreach arr {::Config ::Httpd} {
	$slave eval [list array set $arr [array get $arr]]
    }
    $slave eval [list array set ::Httpd [list name $host]]
    # Load the packages
    $slave eval package require httpd [package provide httpd]
    foreach pkg {version utils counter config} {
	$slave eval \
		package require httpd::$pkg [package provide httpd::$pkg]
    }
    $slave eval [list array set Config [list config $file host $host]]
    $slave eval {
	config::init $Config(config) Config
	namespace import config::cget

	# This replaces the command line processing
	array set Config [array get config::Config]

	if {[string length $Config(library)] &&
		[lsearch -exact $auto_path $Config(library)] == -1} {
	    lappend auto_path $Config(library)
	}
	Httpd_Init

	if {$Config(threads) > 0} {
	    package require Thread		;# C extension
	    package require httpd::threadmgr	;# Tcl layer on top
	    Thread_Init $Config(threads)
	} else {
	    # Stub out Thread_Respond so threadmgr isn't required
	    proc Thread_Respond {args} {return 0}
	    proc Thread_Enabled {} {return 0}
	}
	source $Config(main)
	Log_SetFile		[cget LogFile]$Config(port)_
	Log_FlushMinutes	[cget LogFlushMinutes]
	Log_Flush
    }

    foreach host $hostNames {
        set host [string tolower $host]
	set virtual($host) $slave
    }

}

# Httpd_SecureServer --
#
#	Like Httpd_Server, but with additional setup for SSL.
#	This requires the TLS extension.
#
# Arguments:
#	port	The TCP listening port number
#	name	The qualified host name returned in the Host field.  Defaults
#		to [info hostname]
#	ipaddr	Non-default interface address.  Otherwise IP_ADDR_ANY is used
#		so the server can accept connections from any interface.
#
# Results:
#	none
#
# Side Effects:
#	This sets up a callback to HttpdAccept for new connections.

proc Httpd_SecureServer {{port 443} {name {}} {ipaddr {}}} {
    global Httpd

    if {![info exists Httpd(initialized)]} {
	Httpd_Init
    }
    catch {close $Httpd(https_listen)}
    set Httpd(name) $name
    set Httpd(https_ipaddr) $ipaddr
    set Httpd(https_port) $port
    if {[string length $name] == 0} {
	set Httpd(name) [info hostname]
    }
    package require tls

    # This now depends on a call to tls::init being made elsewhere, typically
    # in the main startup script.  That call sets all the various SSL parameters
    # based on the server's configuration file.

    set cmd [list tls::socket -server [list HttpdAccept \
	    [list https $name $port]]]
    if {[string length $ipaddr] != 0} {
        lappend cmd -myaddr $ipaddr
    }
    lappend cmd $port
    if {[catch $cmd Httpd(https_listen)]} {
        return -code error "$Httpd(name):$port $Httpd(https_listen)\ncmd=$cmd"
    }
}

# Httpd_SecureServerShutdown --
#
#	Close the server's secure socket.
#
# Arguments:
#	none
#
# Results:
#	Returns "" if the socket was successfully closed, otherwise an error string.
#
# Side Effects:
#	Close the server's HTTPS listening socket.

proc Httpd_SecureServerShutdown {} {
    global Httpd
    Log {} ShutdownSecureSocket
    catch {close $Httpd(https_listen)} err
    return $err
}

# Httpd_Shutdown --
#
#	Kill the server gracefully
#
# Arguments:
#	none
#
# Results:
#	none
#
# Side Effects:
#	Close the server listening socket(s)
#	Invoke any registered shutdown procedures.

proc Httpd_Shutdown {} {
    global Httpd
    variable virtual
    set ok 1
    foreach host [array names virtual] {
	$virtual($host) eval Httpd_Shutdown
    }
    foreach handler $Httpd(shutdown) {
	if {[catch {eval $handler} err]} {
	    Log "" "Shutdown: $handler" $err
	    set ok 0
	}
    }
    Log {} Shutdown
    Httpd_ServerShutdown
    Httpd_SecureServerShutdown
    return $ok
}

# Httpd_RegisterShutdown --
#
#	Register a Tcl command to be called by Httpd_Shutdown
#
# Arguments:
#	cmd	The command to eval from Httpd_Shutdown
#
# Results:
#	none
#
# Side Effects:
#	Save the callback.

proc Httpd_RegisterShutdown {cmd} {
    global Httpd
    if {[lsearch $Httpd(shutdown) $cmd] < 0} {
	lappend Httpd(shutdown) $cmd
    }
}

# HttpdAccept --
#
#	This is the socket accept callback invoked by Tcl when
#	clients connect to the server.
#
# Arguments:
#	self	A list of {protocol name port} that identifies the server
#	sock	The new socket connection
#	ipaddr	The client's IP address
#	port	The client's port
#
# Results:
#	none
#
# Side Effects:
#	Set up a handler, HttpdRead, to read the request from the client.
#	The per-connection state is kept in Httpd$sock, (e.g., Httpdsock6),
#	and upvar is used to create a local "data" alias for this global array.

proc HttpdAccept {self sock ipaddr port} {
    global Httpd
    upvar #0 Httpd$sock data

    Count accepts
    Count sockets
    set data(self) $self
    set data(ipaddr) $ipaddr
    if {[Httpd_Protocol $sock] == "https"} {
	
	# There is still a lengthy handshake that must occur.
	# We do that by calling tls::handshake in a fileevent
	# until it is complete, or an error occurs.

	Count accept_https
	fconfigure $sock -blocking 0
	fileevent $sock readable [list HttpdHandshake $sock]
    } else {
	HttpdReset $sock $Httpd(maxused)
    }
}

# HttpdHandshake --
#
#	Complete the SSL handshake. This is called from a fileevent
#	on a new https connection.  It calls tls::handshake until done.
#
# Arguments:
#	sock	The socket connection
#
# Results:
#	none
#
# Side Effects:
#	If the handshake fails, close the connection.
#	Otherwise, call HttpdReset to set up the normal HTTP protocol.

proc HttpdHandshake {sock} {
    upvar #0 Httpd$sock data
    global Httpd errorCode
	
    if {[catch {tls::handshake $sock} complete]} {
	if {[lindex $errorCode 1] == "EAGAIN"} {
	    # This seems to occur normally on UNIX systems
	    return
	}
	Log $sock "HttpdHandshake" "\{$data(self)\} $sock \
	    $data(ipaddr) $complete"
	Httpd_SockClose $sock 1 "$complete"
    } elseif {$complete} {
	set data(cert) [tls::status $sock]
	HttpdReset $sock $Httpd(maxused)
    }
}

# HttpdReset --
#
#	Initialize or reset the socket state.
#	We allow multiple transactions per socket (keep alive).
#
# Arguments:
#	sock	The socket connection
#	left	(optional) The keepalive connection count.
#
# Results:
#	none
#
# Side Effects:
#	Resets the "data" array.
#	Cancels any after events.
#	Closes the socket upon error or if the reuse counter goes to 0.
#	Sets up the fileevent for HttpdRead

proc HttpdReset {sock {left {}}} {
    global Httpd
    upvar #0 Httpd$sock data

    if {[catch {
	flush $sock
    } err]} {
	Httpd_SockClose $sock 1 $err
	return
    }
    Count connections

    # Count down transactions.

    if {[string length $left]} {
	set data(left) $left
    } else {
	set left [incr data(left) -1]
    }
    if {[info exists data(cancel)]} {
	after cancel $data(cancel)
    }

    # Clear out (most of) the data array.

    set ipaddr $data(ipaddr)
    set self $data(self)
    if {[info exist data(cert)]} {
	set cert $data(cert)
    }
    unset data
    array set data [list state start version 0 \
	    left $left ipaddr $ipaddr self $self]
    if {[info exist cert]} {
	set data(cert) $cert
    }

    # Set up a timer to close the socket if the next request
    # is not completed soon enough.  The request has already
    # been started, but a bad URL domain might not finish.

    set data(cancel) [after $Httpd(timeout1) \
	[list Httpd_SockClose $sock 1 "timeout"]]
    fconfigure $sock -blocking 0 -buffersize $Httpd(bufsize) \
	-translation {auto crlf}
    fileevent $sock readable [list HttpdRead $sock]
    fileevent $sock writable {}
}

# Httpd_Peername --
#
# Really need to fix the core to support DNS lookups.
# This routine is not used anywhere.
#
# Arguments:
#	sock	Socket connection
#
# Results:
#	The clients dns name.
#
# Side Effects:
#	None

proc Httpd_Peername {sock} {
    # This is expensive!
    fconfigure $sock -peername
}

# HttpdRead --
#
# Read request from a client.  This is the main state machine
# for the protocol.
#
# Arguments:
#	sock	Socket connection
#
# Results:
#	None
#
# Side Effects:
#	Reads the request from the socket and dispatches the
#	URL request when ready.

proc HttpdRead {sock} {
    global Httpd
    upvar #0 Httpd$sock data

    # Use line mode to read the request and the mime headers

    if {[catch {gets $sock line} readCount]} {
	Httpd_SockClose $sock 1 "read error: $readCount"
	return
    }

    # State machine is a function of our state variable:
    #	start: the connection is new
    #	mime: we are reading the protocol headers
    # and how much was read. Note that
    # [string compare $readCount 0] maps -1 to -1, 0 to 0, and > 0 to 1
    set state [string compare $readCount 0],$data(state)

    switch -glob -- $state {
	1,start	{
	    if {[regexp {^([^ ]+) +([^?]+)\??([^ ]*) +HTTP/(1.[01])} \
		    $line x data(proto) data(url) data(query) data(version)]} {
		
		# data(uri) is the complete URI

		set data(uri) $data(url)
		if {[string length $data(query)]} {
		    append data(uri) ?$data(query)
		}

		# Strip leading http://server and look for the proxy case.

		if {[regexp {^https?://([^/:]+)(:([0-9]+))?(.*)$} $data(url) \
			x xserv y xport urlstub]} {
		    set myname [Httpd_Name $sock]
		    set myport [Httpd_Port $sock]
		    if {([string compare \
			    [string tolower $xserv] \
			    [string tolower $myname]] != 0) ||
			    ($myport != $xport)} {
			set data(host) $xserv
			set data(port) $xport
		    }
		    # Strip it out if it is for us (i.e., redundant)
		    # This makes it easier for doc handlers to
		    # look at the "url"
		    set data(url) $urlstub
		}
		set data(state) mime
		set data(line) $line
		CountHist urlhits

		# Limit the time allowed to serve this request

		if {[info exists data(cancel)]} {
		    after cancel $data(cancel)
		}
		set data(cancel) [after $Httpd(timeout2) \
		    [list HttpdCancel $sock]]
	    } else {
		# Could check for FTP requests, here...
		Log $sock HttpError $line
		Httpd_SockClose $sock 1
	    }
	}
	0,start {
	    # This can happen in between requests.
	}
	1,mime	{
	    # This regexp picks up
	    # key: value
	    # MIME headers.  MIME headers may be continue with a line
	    # that starts with spaces.
	    if {[regexp {^([^ :]+):[ 	]*(.*)} $line dummy key value]} {

                # The following allows something to
                # recreate the headers exactly

                lappend data(headerlist) $key $value

                # The rest of this makes it easier to pick out
                # headers from the data(mime,headername) array

		set key [string tolower $key]
		if [info exists data(mime,$key)] {
		    append data(mime,$key) ,$value
		} else {
		    set data(mime,$key) $value
		    lappend data(mimeorder) $key
		}
		set data(key) $key

	    } elseif {[regexp {^[ 	]+(.*)}  $line dummy value]} {
		# Are there really continuation lines in the spec?
		if [info exists data(key)] {
		    append data(mime,$data(key)) " " $value
		} else {
		    Httpd_Error $sock 400 $line
		}
	    } else {
		Httpd_Error $sock 400 $line
	    }
	    # Check for virtual host
	    variable virtual
	    if {[string compare host $key]} {return}
	    set host [lindex [split [string tolower $value] :] 0]
	    if {[catch {set virtual($host)} i]} {return}

	    # Transfer $sock to interp $i
	    fileevent $sock readable {}
	    interp transfer {} $sock $i
	    set data(self) [list [Httpd_Protocol $sock] \
		    $host [Httpd_Port $sock]]
	    if {[info exists data(cancel)]} {
		after cancel $data(cancel)
		unset data(cancel)
	    }
	    $i eval [list array set Httpd$sock [array get data]]
	    unset data
	    $i eval [list fileevent $sock readable [list HttpdRead $sock]]
	    set tmp [$i eval [list \
		    after $Httpd(timeout2) [list HttpdCancel $sock]]]
	    $i eval [list array set Httpd$sock [list cancel $tmp]]
	}
	0,mime	{
	    if {$data(proto) == "POST"} {
		fconfigure $sock  -translation {binary crlf}
		if {![info exists data(mime,content-length)]} {
		    Httpd_Error $sock 411
		    return
		}
		set data(count) $data(mime,content-length)
		if {$data(version) >= 1.1 && [info exists data(mime,expect)]} {
		    if {$data(mime,expect) == "100-continue"} {
			puts $sock "100 Continue HTTP/1.1\n"
			flush $sock
		    } else {
			Httpd_Error $sock 419 $data(mime,expect)
			return
		    }
		}

		# Flag the need to check for an extra newline
		# in SSL connections by some browsers.

		set data(checkNewline) 1

		# Facilitate a backdoor hook between Url_DecodeQuery
		# where it will read the post data on behalf of the
		# domain handler in the case where the domain handler
		# doesn't use an Httpd call to read the post data itself.

		Url_PostHook $sock $data(count)
	    } else {
		Url_PostHook $sock 0    ;# Clear any left-over hook
		set data(count) 0
	    }

	    # Disabling this fileevent makes it possible to use
	    # http::geturl in domain handlers reliably

	    fileevent $sock readable {}

	    # The use of HTTP_CHANNEL is a disgusting hack.

	    set ::env(HTTP_CHANNEL) $sock

	    # Do a different dispatch for proxies.  By default, no proxy.

	    if {[info exist data(host)]} {
		if {[catch {
		    Proxy_Dispatch $sock
		} err]} {
		    Httpd_Error $sock 400 "No proxy support\n$err"
		}
	    } else {
		# Dispatch to the URL implementation.

		# As a service for domains that loose track of their
		# context (e.g., .tml pages) we save the socket in a global.
		# If a domain implementation would block and re-enter the
		# event loop, it must use Httpd_Suspend to clear this state,
		# and use Httpd_Resume later to restore it.

		set Httpd(currentSocket) $sock
		CountStart serviceTime $sock
		Url_Dispatch $sock
	    }
	}
	-1,* {
	    if {[fblocked $sock]} {
		# Blocked before getting a whole line
		return
	    }
	    if {[eof $sock]} {
		Httpd_SockClose $sock 1 ""
		return
	    }
	}
	default {
	    Httpd_Error $sock 404 "$state ?? [expr {[eof $sock] ? "EOF" : ""}]"
	}
    }
}

# Httpd_PostDataSize --
#
# Arguments:
#	sock	Client connection
#
# Results:
#	The amount of post data available.

proc Httpd_PostDataSize {sock} {
    upvar #0 Httpd$sock data

    return $data(count)
}

# Httpd_GetPostData --
#
# Arguments:
#	sock	Client connection
#	varName	Name of buffer variable to append post data to
#	size	Amount of data to read this call. -1 to read all available.
#
# Results:
#	The amount of data left to read.  When this goes to zero, you are done.

proc Httpd_GetPostData {sock varName {size -1}} {
    global Httpd
    upvar #0 Httpd$sock data
    upvar 1 $varName buffer

    if {$size < 0} {
	set size $Httpd(bufsize)
    }
    HttpdReadPost $sock buffer $size
    return $data(count)
}

# Httpd_ReadPostDataAsync --
#
#	Convenience layer on Http-GetPostDataAsync to
#	read the POST data into a the data(query) variable.
#
# Arguments:
#	(Same as HttpdReadPost)
#
# Side Effects:
#	(See Httpd_GetPostDataAsync)

proc Httpd_ReadPostDataAsync {sock cmd} {
    global Httpd
    upvar #0 Httpd$sock data
    if {[string length $data(query)]} {
	# This merges query data from the GET/POST URL
	append data(query) &
    }
    Httpd_Suspend $sock
    fileevent $sock readable [list HttpdReadPostGlobal $sock \
	    Httpd${sock}(query) $Httpd(bufsize) $cmd]
    return
}

# Httpd_GetPostDataAsync --
#
#	Read the POST data into a Tcl variable, but do it in the
#	background so the server doesn't block on the socket.
#
# Arguments:
#	(Same as HttpdReadPost)
#
# Side Effects:
#	This schedules a readable fileevent to read all the POST data
#	asynchronously.  The data is appened to the named variable.
#	The callback is made 

proc Httpd_GetPostDataAsync {sock varName blockSize cmd} {
    Httpd_Suspend $sock
    fileevent $sock readable \
	[list HttpdReadPostGlobal $sock $varName $blockSize $cmd]
    return
}

# HttpdReadPostGlobal --
#
# This fileevent callback can only access a global variable.
# But HttpdReadPost needs to affect a local variable in its
# caller so it can be shared with Httpd_GetPostData.
# So, the fileevent case has an extra procedure frame.
#
# Arguments:
#	(Same as HttpdReadPost)
#
# Results:
#	None
#
# Side Effects:
#	Accumulates POST data into the named variable

proc HttpdReadPostGlobal {sock varName blockSize {cmd {}}} {
    upvar #0 $varName buffer
    HttpdReadPost $sock buffer $blockSize $cmd
}

# HttpdReadPost --
#
#	The core procedure that reads post data and accumulates it
#	into a Tcl variable.
#
# Arguments:
#	sock	Client connection
#	varName	Name of buffer variable to append post data to.  This
#		must be a global or fully scoped namespace variable, or
#		this can be the empty string, in which case the data
#		is discarded.
#	blockSize	Default read block size.
#	cmd	Callback to make when the post data has been read.
#		It is called like this:
#		cmd $sock $varName $errorString
#		Where the errorString is only passed if an error occurred.
#
# Results:
#	None
#
# Side Effects:
#	Consumes post data and appends it to a variable.

proc HttpdReadPost {sock varName blockSize {cmd {}}} {
    global Httpd
    upvar #0 Httpd$sock data

    # Ensure that the variable, if specified, exists by appending "" to it

    if {[string length $varName]} {
	upvar 1 $varName buffer
	append buffer ""
    }

    if {[eof $sock]} {
	if {$data(count)} {
	    set doneMsg "Short read: got [string length $buffer] bytes,\
		expected $data(count) more bytes"
	    set data(count) 0
	} else {
	    set doneMsg ""
	}
    } else {
	if {[info exist data(checkNewline)]} {

	    # Gobble a single leading \n from the POST data
	    # This is generated by various versions of Netscape
	    # when using https/SSL.  This extra \n is not counted
	    # in the content-length (thanks!)

	    set nl [read $sock 1]
	    if {[string compare $nl \n] != 0} {

		# It was not an extra newline.

		incr data(count) -1
		if {[info exist buffer]} {
		    append buffer $nl
		}
	    }
	    unset data(checkNewline)
	}
	set toRead [expr {$data(count) > $blockSize ? \
		$blockSize : $data(count)}]
	if {[catch {read $sock $toRead} block]} {
	    set doneMsg $block
	    set data(count) 0
	} else {
	    if {[info exist buffer]} {
		append buffer $block
	    }

	    set data(count) [expr {$data(count) - [string length $block]}]
	    if {$data(count) == 0} {
		set doneMsg ""
	    }
	}
    }
    if {[info exist doneMsg]} {
	Url_PostHook $sock 0
	catch {fileevent $sock readable {}}
	Httpd_Resume $sock
	if {[string length $cmd]} {
	    eval $cmd [list $sock $varName $doneMsg]
	}
	return $doneMsg
    } else {
	return ""
    }
}

# Httpd_CopyPostData --
#
#	Copy the POST data to a channel and make a callback when that
#	has completed.
#
# Arguments:
#	sock	Client connection
#	channel	Channel, e.g., to a local file or to a proxy socket.
#	cmd	Callback to make when the post data has been read.
#		It is called like this:
#		    cmd $sock $channel $bytes $errorString
#		Bytes is the number of bytes transferred by fcopy.
#		errorString is only passed if an error occurred,
#		otherwise it is an empty string
#
# Side Effects:
#	This uses fcopy to transfer the data from the socket to the channel.

proc Httpd_CopyPostData {sock channel cmd} {
    upvar #0 Httpd$sock data
    fcopy $sock $channel -size $data(count) \
    	-command [concat $cmd $sock $channel]
    Url_PostHook $sock 0
    return
}

# Httpd_GetPostChannel --
#
# Arguments:
#	sock		Client connection
#	sizeName	Name of variable to get the amount of post
#			data expected to be read from the channel
#
# Results:
#	The socket, as long as there is POST data to read

proc Httpd_GetPostChannel {sock sizeName} {
    upvar #0 Httpd$sock data
    upvar 1 $sizeName size

    if {$data(count) == 0} {
	error "no post data"
    }
    set size $data(count)
    return $sock
}

# The following are several routines that return replies

# HttpdCloseP --
#	See if we should close the socket
#
# Arguments:
#	sock	the connection handle
#
# Results:
#	1 if the connection should be closed now, 0 if keep-alive

proc HttpdCloseP {sock} {
    upvar #0 Httpd$sock data

    if {[info exists data(mime,connection)]} {
	if {[string tolower $data(mime,connection)] == "keep-alive"} {
	    Count keepalive
	    set close 0
	} else {
	    Count connclose
	    set close 1
	}
    } elseif {[info exists data(mime,proxy-connection)]} {
	if {[string tolower $data(mime,proxy-connection)] == "keep-alive"} {
	    Count keepalive
	    set close 0
	} else {
	    Count connclose
	    set close 1
	}
    } elseif {$data(version) >= 1.1} {
	Count http1.1
    	set close 0
    } else {
	# HTTP/1.0
	Count http1.0
	set close 1
    }
    if {[expr {$data(left) == 0}]} {
	# Exceeded transactions per connection
	Count noneleft
    	set close 1
    }
    return $close
}

# Httpd_CompletionCallback --
#
#	Register a procedure to be called when an HTTP request is
#	completed, either normally or forcibly closed.  This gives a
#	URL implementation a guaranteed callback to clean up or log
#	requests.
#
# Arguments:
#	sock	The connection handle
#	cmd	The callback to make.  These arguments are added:
#		sock - the connection
#		errmsg - An empty string, or an error message.
#
# Side Effects:
# 	Registers the callback

proc Httpd_CompletionCallback {sock cmd} {
    upvar #0 Httpd$sock data
    set data(callback) $cmd
}

# HttpdDoCallback --
#
#	Invoke the completion callback.
#
# Arguments:
#	sock	The connection handle
#	errmsg	The empty string, or an error message.
#
# Side Effects:
# 	Invokes the callback

proc HttpdDoCallback {sock {errmsg {}}} {
    upvar #0 Httpd$sock data
    if {[info exists data(callback)]} {
	catch {eval $data(callback) {$sock $errmsg}}

	# Ensure it is only called once
	unset data(callback)
    }
    CountStop serviceTime $sock
}

# Httpd_AddHeaders
#	Add http headers to be used in a reply
#	Call this before using Httpd_ReturnFile or
#	Httpd_ReturnData
#
# Arguments:
#	sock	handle on the connection
#	args	a list of header value ...

proc Httpd_AddHeaders {sock args} {
    upvar #0 Httpd$sock data

    eval lappend data(headers) $args
}

# Httpd_RemoveHeaders
#	Remove previously set headers from the reply.
#	Any headers that match the glob pattern are removed.
#
# Arguments:
#	sock	handle on the connection
#	pattern	glob pattern to match agains cookies.

proc Httpd_RemoveHeaders {sock {pattern *}} {
    upvar #0 Httpd$sock data
    if {[info exists data(headers)] && $data(headers) != {}} {
        set tmp {}
        foreach {header value} $data(headers) {
            if {![string match $pattern $header]} {
                lappend tmp $header $value
            }
        }
        set data(headers) $tmp
    }
    return
}

# Httpd_NoCache
#	Insert header into http header to indicate that this page
#	should not be cached
#
# Arguments:
#	sock	handle on the connection

proc Httpd_NoCache {sock} {
    Httpd_RemoveHeaders $sock Cache-Control
    Httpd_AddHeaders $sock Cache-Control no-cache
    Httpd_AddHeaders $sock Expires content '-1'
}

# Httpd_Refresh
#	Insert header into http header to cause browser to refresh
#	after a delay, optionally to a different URL
#
# Arguments:
#	sock	handle on the connection
#	time	time in seconds before refresh
#	url	optional: url to refresh to

proc Httpd_Refresh {sock time {url ""}} {
    Httpd_RemoveHeaders $sock Cache-Control
    if {$url == ""} {
	Httpd_AddHeaders $sock Refresh $time
    } else {
	Httpd_AddHeaders $sock Refresh ${time}\;url=${url}
    }
}

# HttpdRespondHeader --
#
#	Utility routine for outputting response headers for normal data Does
#	not output the end of header markers so additional header lines can be
#	added
#
# Arguments:
#	sock	The connection handle
#	type	The mime type of this response
#	close	If true, signal connection close headers.  See HttpdCloseP
#	size	The size "in bytes" of the response
#	code	The return code - defualts to 200
#
# Side Effects:
# 	Outputs header lines

proc HttpdRespondHeader {sock type close size {code 200}} {
    global Httpd
    upvar #0 Httpd$sock data

    set data(code) $code
    append reply "HTTP/$data(version) $code [HttpdErrorString $code]" \n
    append reply "Date: [HttpdDate [clock seconds]]" \n
    append reply "Server: $Httpd(server)\n"

    if {$close} {
	append reply "Connection: Close" \n
    } elseif {$data(version) == 1.0 && !$close} {
	append reply "Connection: Keep-Alive" \n
    }
    append reply "Content-Type: $type" \n
    if {[string length $size]} {
	append reply "Content-Length: $size" \n
    }

    if {[info exists data(headers)]} {
	foreach {header value} $data(headers) {
	    catch {
		append reply "[string trimright $header :]: " $value \n
	    }
	}
    }

    puts -nonewline $sock $reply
}

# HttpdErrorString --
#
#	Map from an error code to a meaningful string.
#
# Arguments:
#	code	An HTTP error code, e.g., 200 or 404
#
# Results:
#	An error string, e.g. "Data follows" or "File Not Found"
#
# Side Effects:
# 	None

proc HttpdErrorString { code } {
    global Httpd_Errors
    if {[info exist Httpd_Errors($code)]} {
	return $Httpd_Errors($code)
    } else {
	return "Error $code"
    }
}

# Httpd_RemoveCookies
#	Remove previously set cookies from the reply.
#	Any cookies that match the glob pattern are removed.
#	This is useful for expiring a cookie that was previously set.
#
# Arguments:
#	sock	handle on the connection
#	pattern	glob pattern to match agains cookies.

proc Httpd_RemoveCookies {sock pattern} {
    upvar #0 Httpd$sock data
    if {[info exists data(set-cookie)] && $data(set-cookie) != {}} {
        set tmp {}
        foreach c $data(set-cookie) {
            if {![string match $pattern $c]} {
                lappend tmp $c
            }
        }
        set data(set-cookie) $tmp
    }
    return
}

# Httpd_SetCookie
#	Define a cookie to be used in a reply
#	Call this before using Httpd_ReturnFile or
#	Httpd_ReturnData
#
# Arguments:
#	sock	handle on the connection
#	cookie	Set-Cookie line
#	modify	(optional) If true, overwrite any preexisting
#		cookie that matches.  This way you can change
#		the expiration time.

proc Httpd_SetCookie {sock cookie {modify 0}} {
    upvar #0 Httpd$sock data
    lappend data(set-cookie) $cookie
}

# HttpdSetCookie
#	Generate the Set-Cookie headers in a reply
#	Use Httpd_SetCookie to register cookes eariler
#
# Arguments:
#	sock	handle on the connection

proc HttpdSetCookie {sock} {
    upvar #0 Httpd$sock data
    if {[info exist data(set-cookie)]} {
	foreach item $data(set-cookie) {
	    puts $sock "Set-Cookie: $item"
	}
	# HttpdCookieLog $sock HttpdSetCookie
	unset data(set-cookie)
    }
}

# Httpd_ReturnFile
#	Return a file.
#
# Arguments:
#	sock	handle on the connection
#	type	is a Content-Type
#	path	is the file pathname
#	offset	amount to skip at the start of file
#
# Side Effects:
#	Sends the file contents back as the reply.

proc Httpd_ReturnFile {sock type path {offset 0}} {
    global Httpd
    upvar #0 Httpd$sock data

    if {[Thread_Respond $sock \
	    [list Httpd_ReturnFile $sock $type $path $offset]]} {
	return
    }

    # Set file size early so it gets into all log records

    set data(file_size) [file size $path]
    set data(code) 200

    Count urlreply
    if {[info exists data(mime,if-modified-since)]} {
        # No need for complicated date comparison, if they're identical then 304.
	if {$data(mime,if-modified-since) == [HttpdDate [file mtime $path]]} {
            Httpd_NotModified $sock
            return
        }
    } 

    # Some files have a duality, when the client sees X bytes but the
    # file is really X + n bytes (the first n bytes reserved for server
    # side accounting information.

    incr data(file_size) -$offset

    if {[catch {
	set close [HttpdCloseP $sock]
	HttpdRespondHeader $sock $type $close $data(file_size) 200
	HttpdSetCookie $sock
	puts $sock "Last-Modified: [HttpdDate [file mtime $path]]"
	puts $sock ""
	if {$data(proto) != "HEAD"} {
	    set in [open $path]		;# checking should already be done
	    fconfigure $in -translation binary -blocking 1
	    if {$offset != 0} {
		seek $in $offset
	    }
	    fconfigure $sock -translation binary -blocking $Httpd(sockblock)
	    set data(infile) $in
	    Httpd_Suspend $sock 0
	    fcopy $in $sock -command [list HttpdCopyDone $in $sock $close]
	} else {
	    Httpd_SockClose $sock $close
	}
    } err]} {
	HttpdCloseFinal $sock $err
    }
}

# Httpd_ReturnData
#	Return data for a page.
#
# Arguments:
#	sock	handle on the connection
#	type	a Content-Type
#	content	the data to return
#	code	the HTTP reply code.
#
# Side Effects:
#	Send the data down the socket

proc Httpd_ReturnData {sock type content {code 200} {close 0}} {
    global Httpd
    upvar #0 Httpd$sock data

    if {[Thread_Respond $sock \
	    [list Httpd_ReturnData $sock $type $content $code $close]]} {
	return
    }

    Count urlreply
    if {$close == 0} {
    	set close [HttpdCloseP $sock]
    }
    if {[catch {
	HttpdRespondHeader $sock $type $close [string length $content] $code
	HttpdSetCookie $sock
	puts $sock ""
	if {$data(proto) != "HEAD"} {
	    fconfigure $sock -translation binary -blocking $Httpd(sockblock)
	    puts -nonewline $sock $content
	}
	Httpd_SockClose $sock $close
    } err]} {
	HttpdCloseFinal $sock $err
    }
}

# Httpd_ReturnCacheableData
#	Return data with a Last-Modified time so
#	that proxy servers can cache it.  Or they seem to, anyway.
#
# Arguments:
#	sock	Client connection
#	type	a Content-Type
#	content	the data to return
#	date	Modify date of the date
#	code	the HTTP reply code.
#
# Side Effects:
#	Send the data down the socket

proc Httpd_ReturnCacheableData {sock type content date {code 200}} {
    global Httpd 
    upvar #0 Httpd$sock data

    if {[Thread_Respond $sock \
	    [list Httpd_ReturnCacheableData $sock $type $content $date $code]]} {
	return
    }

    Count urlreply
    set close [HttpdCloseP $sock]
    if {[catch {
	HttpdRespondHeader $sock $type $close [string length $content] $code
	HttpdSetCookie $sock
	puts $sock "Last-Modified: [HttpdDate $date]"
	puts $sock ""
	if {$data(proto) != "HEAD"} {
	    fconfigure $sock -translation binary -blocking $Httpd(sockblock)
	    puts -nonewline $sock $content
	}
	Httpd_SockClose $sock $close
    } err]} {
	HttpdCloseFinal $sock $err
    }
}

# HttpdCopyDone -- this is used with fcopy when the copy completes.
#
# Arguments:
#	in	Input channel, typically a file
#	sock	Socket connection
#	close	If true, the socket is closed after the copy.
#	bytes	How many bytes were copied
#	error	Optional error string.
#
# Results:
#	None
#
# Side Effects:
#	See Httpd_SockClose

proc HttpdCopyDone {in sock close bytes {error {}}} {
    if {$error == ""} {
        # This special value signals a normal close,
        # and triggers a log record so static files are counted
        set error Close
    }
    Httpd_SockClose $sock $close $error
}

# HttpdCancel --
#
# Cancel a transaction if the client doesn't complete the request fast enough.
#
# Arguments:
#	sock	Socket connection
#
# Results:
#	None
#
# Side Effects:
#	Terminates the connection by returning an error page.

proc HttpdCancel {sock} {
    upvar #0 Httpd$sock data
    Count cancel
    Httpd_Error $sock 408
}

# generic error response

set Httpd_ErrorFormat {
    <title>Httpd_Error: %1$s</title>
    Got the error <b>%2$s</b><br>
    while trying to obtain <b>%3$s</b>.
}

# Httpd_Error --
#
# send the error message, log it, and close the socket.
# Note that the Doc module tries to present a more palatable
# error display page, but falls back to this if necessary.
#
# Arguments:
#	sock	Socket connection
#	code	HTTP error code, e.g., 500
#	detail  Optional string to append to standard error message.
#
# Results:
#	None
#
# Side Effects:
#	Generates a HTTP response.

proc Httpd_Error {sock code {detail ""}} {

    if {[Thread_Respond $sock [list Httpd_Error $sock $code $detail]]} {
	# We've passed the error back to the main thread
	return
    }

    upvar #0 Httpd$sock data
    global Httpd Httpd_ErrorFormat

    Count errors
    append data(url) ""
    set message [format $Httpd_ErrorFormat $code [HttpdErrorString $code] $data(url)]
    append message <br>$detail
    if {$code == 500} {
	append message "<h2>Tcl Call Trace</h2>"
	for {set l [expr [info level]-1]} {$l > 0} {incr l -1} {
	    append message "$l: [protect_text [info level $l]]<br>"
	}
    }
    Log $sock Error $code $data(url) $detail

    # We know something is bad here, so we make the completion callback
    # and then unregister it so we don't get an extra call as a side
    # effect of trying to reply.

    HttpdDoCallback $sock $message

    if {[info exists data(infile)]} {
	# We've already started a reply, so just bail out
	Httpd_SockClose $sock 1
	return
    }
    if [catch {
	HttpdRespondHeader $sock text/html 1 [expr {[string length $message] + 4}] $code
	puts $sock ""
	puts $sock $message
    } err] {
	Log $sock LostSocket $data(url) $err
    }
    Httpd_SockClose $sock 1
}

set HttpdRedirectFormat {
    <html><head>
    <title>Found</title>
    </head><body>
    This document has moved to a new <a href="%s">location</a>.
    Please update your documents and hotlists accordingly.
    </body></html>
}

# Httpd_Redirect --
#
# Generate a redirect reply (code 302)
#
# Arguments:
#	newurl	New URL to redirect to.
#	sock	Socket connection
#
# Results:
#	None
#
# Side Effects:
#	Generates an HTTP reply

proc Httpd_Redirect {newurl sock} {
    upvar #0 Httpd$sock data
    global Httpd HttpdRedirectFormat

    if {[Thread_Respond $sock \
	    [list Httpd_Redirect $newurl $sock]]} {
	return
    }

    set message [format $HttpdRedirectFormat $newurl]
    set close [HttpdCloseP $sock]
    HttpdRespondHeader $sock text/html $close [string length $message] 302
    HttpdSetCookie $sock
    puts $sock "Location: $newurl"
    puts $sock "URI: $newurl"
    puts $sock ""

    # The -nonewline is important here to work properly with
    # keep-alive connections

    puts -nonewline $sock $message
    Httpd_SockClose $sock $close
}

# Httpd_NotModified --
#
# Generate a Not Modified reply (code 304)
#
# Arguments:
#	sock	Socket connection
#
# Results:
#	None
#
# Side Effects:
#	Generates an HTTP reply

proc Httpd_NotModified {sock} {
    upvar #0 Httpd$sock data

    if {[Thread_Respond $sock \
	    [list Httpd_NotModified $sock]]} {
	return
    }

    set message [HttpdErrorString 304]
    set close 1
    HttpdRespondHeader $sock text/html $close [string length $message] 304
    puts $sock ""

    # The -nonewline is important here to work properly with
    # keep-alive connections

    puts -nonewline $sock $message
    Httpd_SockClose $sock $close
}

# Httpd_RedirectSelf --
#
# Generate a redirect to another URL on this server.
#
# Arguments:
#	newurl	Server-relative URL to redirect to.
#	sock	Socket connection
#
# Results:
#	None
#
# Side Effects:
#	Generates an HTTP reply.

proc Httpd_RedirectSelf {newurl sock} {
    Httpd_Redirect [Httpd_SelfUrl $newurl $sock] $sock
}

# Httpd_SelfUrl --
#
# Create an absolute URL for this server
#
# Arguments:
#	url	A server-relative URL on this server.
#	sock	The current connection so we can tell if it
#		is the regular port or the secure port.
#
# Results:
#	An absolute URL.
#
# Side Effects:
#	None

proc Httpd_SelfUrl {url {sock ""}} {
    global Httpd
    if {$sock == ""} {
	set sock $Httpd(currentSocket)
    }
    upvar #0 Httpd$sock data

    set type [Httpd_Protocol $sock]
    set port [Httpd_Port $sock]
    if {[info exists data(mime,host)]} {

	# Use in preference to our "true" name because
	# the client might not have a DNS entry for use.

	set name $data(mime,host)
    } else {
	set name [Httpd_Name $sock]
    }
    set newurl $type://$name
    if {[string first : $name] == -1} {
	# Add in the port number, which may or may not be present in
	# the name already.  IE5 sticks the port into the Host: header,
	# while Tcl's own http package does not...

	if {$type == "http" && $port != 80} {
	    append newurl :$port
	}
	if {$type == "https" && $port != 443} {
	    append newurl :$port
	}
    }
    append newurl $url
}

# Httpd_Protocol --
#
# Return the protocol for the connection
#
# Arguments:
#	sock	Socket connection
#
# Results:
#	Either "http" or "https"
#
# Side Effects:
#	None

proc Httpd_Protocol {sock} {
    upvar #0 Httpd$sock data
    return [lindex $data(self) 0]
}

# Httpd_Name --
#
# Return the server name for the connection
#
# Arguments:
#	sock	Socket connection
#
# Results:
#	The name of the server, e.g., www.tcltk.org
#
# Side Effects:
#	None

proc Httpd_Name {sock} {
    upvar #0 Httpd$sock data
    return [lindex $data(self) 1]
}

# Httpd_Port --
#
# Return the port for the connection
#
# Arguments:
#	sock	The current connection. If empty, then the
#		regular (non-secure) port is returned.  Otherwise
#		the port of this connection is returned.
#
# Results:
#	The server's port.
#
# Side Effects:
#	None

proc Httpd_Port {{sock {}}} {
    if {[string length $sock]} {

	# Return the port for this connection

	upvar #0 Httpd$sock data
	return [lindex $data(self) 2]
    } else {

	# Return the non-secure listening port

	global Httpd
	if {[info exist Httpd(port)]} {
	    return $Httpd(port)
	} else {
	    return {}
	}
    }
}
# Httpd_SecurePort --
#
#	Return the secure port of this server
#
# Arguments:
#
# Results:
#	The server's secure port.
#
# Side Effects:
#	None

proc Httpd_SecurePort {} {

    # Return the secure listening port

    global Httpd
    if {[info exist Httpd(https_port)]} {
	return $Httpd(https_port)
    } else {
	return {}
    }
}


# Httpd_RedirectDir --
#
# Generate a redirect because the trailing slash isn't present
# on a URL that corresponds to a directory.
#
# Arguments:
#	sock	Socket connection
#
# Results:
#	None
#
# Side Effects:
#	Generate a redirect page.

proc Httpd_RedirectDir {sock} {
    global Httpd
    upvar #0 Httpd$sock data
    set url  $data(url)/
    if {[info exist data(query)] && [string length $data(query)]} {
	append url ?$data(query)
    }
    Httpd_Redirect $url $sock
}

set HttpdAuthorizationFormat {
    <HTML><HEAD>
    <TITLE>401 Authorization Required</TITLE>
    </HEAD><BODY>
    <H1>Authorization Required</H1>
    This server could not verify that you
    are authorized to access the document you
    requested.  Either you supplied the wrong
    credentials (e.g., bad password), or your
    browser doesn't understand how to supply
    the credentials required.<P>
    </BODY></HTML>
}

# Httpd_RequestAuth --
#
# Generate the (401) Authorization required reply
#
# Arguments:
#	sock	Socket connection
#	type	usually "Basic"
#	realm	browsers use this to cache credentials
#	args	additional name value pairs for request
#
# Results:
#	None
#
# Side Effects:
#	Generate an authorization challenge response.

proc Httpd_RequestAuth {sock type realm args} {
    upvar #0 Httpd$sock data
    global Httpd HttpdAuthorizationFormat

    if {[Thread_Respond $sock \
	    [list Httpd_RequestAuth $sock $type $realm]]} {
	return
    }

    set additional ""
    foreach {name value} $args {
	append additional ", " ${name}=$value
    }

    set close [HttpdCloseP $sock]
    HttpdRespondHeader $sock text/html $close [string length $HttpdAuthorizationFormat] 401
    puts $sock "Www-Authenticate: $type realm=\"$realm\" $additional"
    puts $sock ""
    puts -nonewline $sock $HttpdAuthorizationFormat
    Httpd_SockClose $sock $close
}

# HttpdDate --
#
# generate a date string in HTTP format
#
# Arguments:
#	seconds	Clock seconds value
#
# Results:
#	A date string.
#
# Side Effects:
#	None

proc HttpdDate {seconds} {
    return [clock format $seconds -format {%a, %d %b %Y %T GMT} -gmt true]
}

# Httpd_SockClose --
#	"Close" a connection, although the socket might actually
#	remain open for a keep-alive connection.
#	This means the HTTP transaction is fully complete.
#
# Arguments:
#	sock	Identifies the client connection
#	closeit	1 if the socket should close no matter what
#	message	Logging message.  If this is "Close", which is the default,
#		then an entry is made to the standard log.  Otherwise
#		an entry is made to the debug log.
#
# Side Effects:
#	Cleans up all state associated with the connection, including
#	after events for timeouts, the data array, and fileevents.

proc Httpd_SockClose {sock closeit {message Close}} {
    global Httpd
    upvar #0 Httpd$sock data

    if {[string length $message]} {
	Log $sock $message
	if {$message == "Close"} {

	    # This is a normal close.  Any other message
	    # is some sort of error.

	    set message ""
	}
    }
    # Call back to the URL domain implementation so they are
    # sure to see the end of all their HTTP transactions.
    # There is a slight chance of an error reading any un-read
    # Post data, but if the URL domain didn't want to read it,
    # then they obviously don't care.

    HttpdDoCallback $sock $message

    Count connections -1
    if {[info exist data(infile)]} {

	# Close file or CGI pipe.  Still need catch because of CGI pipe.

	catch {close $data(infile)}
    }
    if {$closeit} {
	if {[info exists data(count)] && $data(count) > 0} {

	    # There is unread POST data.  To ensure that the client
	    # can read our reply properly, we must read this data.
	    # The empty variable name causes us to discard the POST data.

	    if {[info exists data(cancel)]} {
		after cancel $data(cancel)
	    }
	    set data(cancel) [after $Httpd(timeout3) \
		[list HttpdCloseFinal $sock "timeout reading extra POST data"]]

	    Httpd_GetPostDataAsync $sock "" $data(count) HttpdReadPostDone
	} else {
	    HttpdCloseFinal $sock
	}
    } else {
	HttpdReset $sock
    }
}

# HttpdReadPostDone --
#
#	Callback is made to this when we are done cleaning up any
#	unread post data.
#
# Arguments:
#	sock	Socket connection
#	varname	Name of variable with post data
#	errmsg	If not empty, an error occurred on the socket.
#
# Results:
#	None
#
# Side Effects:
#	None here, but see HttpdCloseFinal.

proc HttpdReadPostDone {sock var errmsg} {
    HttpdCloseFinal $sock $errmsg
}

# HttpdCloseFinal --
#
#	Central close procedure.  All close operations should funnel
#	through here so that the right cleanup occurs.
#
# Arguments:
#	sock	Socket connection
#	errmsg	If non-empty, then something went wrong on the socket.
#
# Results:
#	None
#
# Side Effects:
#	Cleans up any after event associated with the connection.
#	Closes the socket.
#	Makes the callback to the URL domain implementation.

proc HttpdCloseFinal {sock {errmsg {}}} {
    upvar #0 Httpd$sock data
    Count sockets -1
    if {[info exists data(cancel)]} {
	after cancel $data(cancel)
    }
    if {[catch {close $sock} err]} {
	Log $sock CloseError $err
	if {[string length $errmsg] == 0} {
	    set errmsg $err
	}
    }
    HttpdDoCallback $sock $errmsg
    if {[info exist data]} {
	unset data
    }
}

# Httpd_RequestComplete --
#
#	Detect if a request has been sent.  The holder of a socket
#	might need to know of the URL request was completed with
#	one of the return-data commands, or is still lingering open.
#
# Arguments:
#	sock	Socket connection
#
# Results:
#	1 if the request was completed, 0 otherwise.
#
# Side Effects:
#	None.

proc Httpd_RequestComplete {sock} {
    upvar #0 Httpd$sock data
    if {![info exist data(state)] || $data(state) == "start"} {
	
	# The connection was closed or reset in a keep-alive situation.

	return 1
    } else {
	return 0
    }
}

# server specific version of bgerror - indent to hide from indexer

    proc bgerror {msg} {
	global errorInfo

	set msg "[clock format [clock seconds]]\n$errorInfo"
	if [catch {Log nosock bgerror $msg}] {
	    Stderr $msg
	}
    }

# HttpdCookieLog --
#
#	Special loggin procedure to debug cookie implementation.
#
# Arguments:
#	sock	Socket connection
#	what	What procedure is doing the logging.
#
# Results:
#	None
#
# Side Effects:
#	Writes to the debug log.

proc HttpdCookieLog {sock what} {
    global Log Httpd
    upvar #0 Httpd$sock data
    if {[info exist Log(log)] && ![info exist Httpd(cookie_log)]} {
	set Httpd(cookie_log) [open $Log(log)cookie a]
    }
    if {[info exist Httpd(cookie_log)]} {
	append result [LogValue data(ipaddr)]
	append result { } \[[clock format [clock seconds] -format %d/%h/%Y:%T] -0700\]

	append result { } $what
	switch $what {
	    Httpd_Dispatch {
		if {![info exist data(mime,cookie)]} {
		    return
		}
		append result { } \"$data(mime,cookie)\"
	    }
	    Httpd_SetCookie -
	    HttpdSetCookie {
		append result { } \"[LogValue data(set-cookie)]\"
	    }
	}
	catch { puts $Httpd(cookie_log) $result ; flush $Httpd(cookie_log)}
    }
}

# Httpd_Suspend --
#
#	Suspend Wire Callback - for async transactions
#	Use Httpd_Resume once you are back in business
#	Note: global page array is not preserved over suspend
#
# Arguments:
#	sock	Socket connection
#	timeout Timeout period.  After this the request is aborted.
#
# Results:
#	None
#
# Side Effects:
#	Disables fileevents and sets up a timer.

proc Httpd_Suspend {sock {timeout ""}} {
    global Httpd
    upvar #0 Httpd$sock data

    fileevent $sock readable {}
    fileevent $sock writable {}

    if {[info exists data(cancel)]} {
	after cancel $data(cancel)
	unset data(cancel)
    }
    if {[info exists Httpd(currentSocket)]} {
	unset Httpd(currentSocket)
    }
    if {$timeout == ""} {
	set timeout $Httpd(timeout2)
    }
    if {$timeout != 0} {
	set data(cancel) [after $timeout [list HttpdCancel $sock]]
    }
}

# Httpd_Resume --
#
#	Resume processing of a request.  Sets up a bit of global state that
#	has been cleared by Httpd_Suspend.
#
# Arguments:
#	sock	Socket connection
#	timeout Timeout period.  After this the request is aborted.
#
# Results:
#	None
#
# Side Effects:
#	Restores the Httpd(currentSocket) setting.

proc Httpd_Resume {sock {timeout ""}} {
    upvar #0 Httpd$sock data
    global Httpd
    set Httpd(currentSocket) $sock
    if {[info exists data(cancel)]} {
	after cancel $data(cancel)
    }
    if {$timeout == ""} {
	set timeout $Httpd(timeout1)
    }
    set data(cancel) [after $timeout \
	[list Httpd_SockClose $sock 1 "timeout"]]
}

# Httpd_CurrentSocket --
#
#	Return (or set) the handle to the current socket.
#
# Arguments:
#	sock	Optional - if specified, set the current socket.
#
# Results:
#	A socket.
#
# Side Effects:
#	None.

proc Httpd_CurrentSocket {{sock {}}} {
    global Httpd
    if {[string length $sock]} {
	set Httpd(currentSocket) $sock
    }
    return $Httpd(currentSocket)
}

# Httpd_Pair --
#
#
# Pair two fd's - typically for tunnelling
# Close both if either one closes (or gets an error)
#
#
# Arguments:
#	sock	Socket connection
#	fd	Any other I/O connection
#
# Results:
#	None
#
# Side Effects:
#	Sets up fileevents for proxy'ing data.

proc Httpd_Pair {sock fd} {
    global Httpd
    upvar #0 Httpd$sock data

    syslog debug "HTTP: Pairing $sock and $fd"

    Httpd_Suspend $sock 0

    fconfigure $sock -translation binary -blocking 0
    fconfigure $fd -translation binary -blocking 0

    fileevent $sock readable [list HttpdReflect $sock $fd]
    fileevent $fd readable [list HttpdReflect $fd $sock]
}

# HttpdReflect --
#
#	This is logically fcopy in both directions, but the core
#	prevents us from doing that so we do it by hand.
#
# Arguments:
#	in	Input channel
#	out	Output channel
#
# Results:
#	None
#
# Side Effects:
#	Copy data between channels.

proc HttpdReflect {in out} {
    global Httpd
    if {[catch {
	set buf [read $in $Httpd(bufsize)]
	puts -nonewline $out $buf
	flush $out
	set buflen [string length $buf]
	if {$buflen > 0} {
	    syslog debug "Tunnel: $in -> $out ($buflen bytes)" 
	}
    } oops]} {
	Log $in Tunnel "Error: $oops"
    } elseif {![eof $in]} {
	return 1
    } else {
	syslog debug "Tunnel: $in EOF"
    }
    fileevent $in readable {}
    fileevent $out readable {}
    catch {flush $in}
    catch {flush $out}
    catch {close $in}
    catch {close $out}
    return 0
}

# Httpd_DumpHeaders --
#
#	Dump out the protocol headers so they can be saved for later.
#
# Arguments:
#	sock	Client connection
#
# Results:
#	A list structure that alternates between names and values.
#	The names are header names without the trailing colon and
#	mapped to lower case (e.g., content-type).  Two pseudo-headers
#	added: One that contains the original request URL; its name is "url"
#	Another that contains the request protocol; its name is "method"
#	There are no duplications in the header keys.  If any headers
#	were repeated, their values were combined by separating them
#	with commas.

proc Httpd_DumpHeaders {sock} {
    upvar #0 Httpd$sock data

    set result [list url $data(uri) method $data(proto) version $data(version)]
    if {[info exist data(mimeorder)]} {
	foreach key $data(mimeorder) {
	    lappend result $key $data(mime,$key)
	}
    }
    return $result
}

# Httpd_Webmaster --
#
# Define an email address for the webmaster
#
# Arguments:
#	email 	The email of the webmaster.  If empty, the
#		current value is returned, which is handy in
#		web pages.
#
# Results:
#	Returns the webmaster email.
#
# Side Effects:
#	Sets the webmaster email.

proc Httpd_Webmaster {{email {}}} {
    global Httpd
    if {[string length $email] == 0} {
	if {![info exists Httpd(webmaster)]} {
	    set Httpd(webmaster) webmaster
	}
	return $Httpd(webmaster)
    } else {
	set Httpd(webmaster) $email
    }
}

# this is too much of a compatibility hassle, so we alias
catch {interp alias {} Doc_Webmaster {} Httpd_Webmaster}

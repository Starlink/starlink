# E.S.O. - VLT project/ESO Archive
# @(#) $Id: ProxyDialog.tcl,v 1.2 1998/10/28 17:37:23 abrighto Exp $
#
# ProxyDialog.tcl - user interface class for defining a proxy server
#                   for HTTP access.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 26 Jun 98   created


itk::usual ProxyDialog {}


itcl::class cat::ProxyDialog {
    inherit util::TopLevelWidget


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }


    # called after options have been evaluated

    protected method init {} {
	global ::env

	wm title $w_ Proxies

	if {"$itk_option(-configfile)" == ""} {
	    config -configfile $env(HOME)/.proxies
	}

	pack [message $w_.proxymsg \
		  -width 6.5i \
		  -padx 1m \
		  -anchor c \
		  -text "If your host is behind a firewall, you may need to use a \
                         proxy server to access remote catalogs via HTTP.\
                         Please enter the hostname and port number for the proxy \
                         server:"] \
	    -side top -anchor w -pady 3m

	pack [set f [frame $w_.proxyf]] \
	    -side top -anchor w -fill x -pady 2m
	
	pack [LabelEntry $w_.proxy \
		  -text "HTTP Proxy server:" \
		  -anchor e \
                  -valuewidth 32] \
	    -side left -fill x -in $f

	pack [LabelEntry $w_.port \
		 -text "Port:" \
		 -anchor e \
		 -valuewidth 8] \
	    -side left -in $f


	pack [message $w_.noproxymsg \
		  -width 6.5i \
		  -padx 1m \
		  -anchor w \
		  -text "Below you can specify a list of domains for which no proxy \
                         server is needed (separate names by space or comma):"] \
	    -side top -anchor w -pady 3m
	
	pack [LabelEntry $w_.noproxy \
		  -text "No proxy for:" \
		  -anchor e] \
	    -side top -fill x -anchor w -pady 2m

	# add buttons
    	pack [frame $w_.buttons -borderwidth 2 -relief raised] \
	    -side top -fill x -pady 2m
	pack \
	    [button $w_.ok \
		 -text "OK" \
		 -command [code $this ok]] \
	    [button $w_.reset \
		 -text "Reset" \
		 -command [code $this reset]] \
	    [button $w_.clear \
		 -text "Clear" \
		 -command [code $this clear]] \
	    [button $w_.cancel \
		 -text "cancel" \
		 -command [code $this cancel]] \
	    -side left -expand 1 -pady 2m -in $w_.buttons

	# set values
	reset
    }

    
    # called when the OK button is pressed

    protected method ok {} {
	global ::env
	set proxy [string trim [$w_.proxy get]]
	set port [string trim [$w_.port get]]
	set noproxy [string trim [$w_.noproxy get]]
	if {"$proxy" != ""} {
	    if {"$port" == ""} {
		set env(http_proxy) "http://$proxy/"
	    } else {
		set env(http_proxy) "http://$proxy:$port/"
	    }
	    set env(http_noproxy) $noproxy
	    set fd [open $itk_option(-configfile) w]
	    puts $fd "http_proxy: $env(http_proxy)"
	    puts $fd "http_noproxy: $env(http_noproxy)"
	    close $fd
	} else {
	    catch {file delete $itk_option(-configfile)}
	    catch {unset env(http_proxy)}
	    catch {unset env(http_noproxy)}
	}
	wm withdraw $w_
    }


    # called when the Reset button is pressed

    protected method reset {} {
	global ::env
	set proxy {}
	set port {}
	set noproxy {}
	if {[info exists env(http_proxy)]} {
	    if {[scan $env(http_proxy) {http://%[^:/]:%d} proxy port] != 2} {
		scan $env(http_proxy) {http://%[^:/]} proxy
	    }
	}
	if {[info exists env(http_noproxy)]} {
	    set noproxy $env(http_noproxy)
	}
	$w_.proxy config -value $proxy
	$w_.port config -value $port
	$w_.noproxy config -value $noproxy
    }


    # called when the Clear button is pressed

    protected method clear {} {
	$w_.proxy config -value {}
	$w_.port config -value {}
	$w_.noproxy config -value {}
    }

    # called when the Cancel button is pressed

    protected method cancel {} {
	wm withdraw $w_
    }


    # Read the given config file (created by this class, see "ok" method)
    # and use it to initialize the environment variables for a proxy server
    # (see tclutil/util/src/HTTP.C).

    public proc check_proxies {file} {
	global ::env
	
	if {[file exists $file]} {
	    set fd [open $file]
	    while {[gets $fd line] != -1} {
		if {[regsub {http_proxy: (.*)} $line {\1} http_proxy]} {
		    set env(http_proxy) $http_proxy
		} elseif {[regsub {http_noproxy: (.*)} $line {\1} http_noproxy]} {
		    set env(http_noproxy) $http_noproxy
		}
	    }
	    close $fd
	}
    }


    # -- options -- 

    # set the width for  displaying labels
    itk_option define -configfile configFile ConfigFile {}
}


# srvui.tcl
# Trivial Tk control panel for the server.
#
# Brent Welch  (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::srvui 1.0

proc SrvUI_Init {title} {
    global Httpd Doc
    option add *font 9x15
    
    wm title . $Httpd(name):$Httpd(port)
    wm protocol . WM_DELETE_WINDOW {Httpd_Shutdown; exit}
    wm iconname . $Httpd(name):$Httpd(port)
	set msgText "$title\n$Httpd(name):$Httpd(port)"
    if {[info exists Httpd(https_listen)]} {
    	append msgText "\n$Httpd(name):$Httpd(https_port) (Secure Server)"
    }
    append msgText "\n$Doc(root)"
    message .msg -text $msgText -aspect 1000
    grid .msg -columnspan 2 -sticky news

    foreach {url label} {
	    / "Home Page"
	    } {
	label .l$url -text $label
	label .n$url -textvariable counterhit($url) -width 0
	grid .l$url .n$url -sticky w
	grid configure .n$url -sticky e
    }
    foreach {counter label} {
	    urlhits "URL Requests"
	    urlreply "URL Replies"
	    cgihits "CGI Hits"
	    maphits "Image Map Hits"
	    errors	"Errors"
	    } {
	label .l$counter -text $label
	label .n$counter -textvariable [CountVarName $counter] -width 0
	grid .l$counter .n$counter -sticky w
	grid configure .n$counter -sticky e
    }
    button .quit -text Quit -command {Httpd_Shutdown ; exit}
    grid .quit -columnspan 2
}

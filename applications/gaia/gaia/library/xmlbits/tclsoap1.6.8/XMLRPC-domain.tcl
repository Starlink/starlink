# XMLRPC-Domain.tcl - Copyright (C) 2004 Pat Thoyts <patthoyts@users.sf.net>
#
# XMLRPC Domain Service module for the tclhttpd web server.
#
# -------------------------------------------------------------------------
# This software is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the accompanying file `LICENSE'
# for more details.
# -------------------------------------------------------------------------

package require SOAP::Domain;		# TclSoap 1.6

namespace eval ::XMLRPC::Domain {
    variable version 1.0;		# package version number

    namespace export register
}

# Register this package with tclhttpd.
#
# This is just a wrapper around SOAP::Domain::register

proc ::XMLRPC::Domain::register {args} {
    eval ::SOAP::Domain::register $args
}

# -------------------------------------------------------------------------

package provide XMLRPC::Domain $XMLRPC::Domain::version

# -------------------------------------------------------------------------

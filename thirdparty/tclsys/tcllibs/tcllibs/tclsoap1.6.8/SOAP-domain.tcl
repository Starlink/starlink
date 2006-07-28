# SOAP-domain.tcl - Copyright (C) 2001 Pat Thoyts <patthoyts@users.sf.net>
#
# SOAP Domain Service module for the tclhttpd web server.
#
# Get the server to require the SOAP::Domain package and call 
# SOAP::Domain::register to register the domain handler with the server.
# ie: put the following in a file in tclhttpd/custom
#    package require SOAP::Domain
#    SOAP::Domain::register -prefix /soap
#
# 3/2004 Pat Thoyts and Jacob Levy: Made to work with xml-rpc and soap
#        and made to load implementations on demand from a specified dir.
#
# -------------------------------------------------------------------------
# This software is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the accompanying file `LICENSE'
# for more details.
# -------------------------------------------------------------------------

package require SOAP::CGI;              # TclSOAP 1.6
package require rpcvar;                 # TclSOAP 1.6
package require log;                    # tcllib 1.0
package require ncgi;			# tcllib 1.0
package require mime;			# tcllib 1.0

namespace eval ::SOAP::Domain {
    variable version 1.4  ;# package version number
    variable debug 0      ;# flag to toggle debug output
    variable rcs_id {$Id$}

    namespace export register

    catch {namespace import -force [namespace parent]::Utils::*}
    catch {namespace import -force [uplevel {namespace current}]::rpcvar::*}
}

# -------------------------------------------------------------------------

# Register this package with tclhttpd.
#
# eg: register -prefix /soap ?-namespace ::zsplat? ?-interp slave?
#
# -prefix is the URL prefix for the SOAP methods to be implemented under
# -interp is the Tcl slave interpreter to use ( {} for the current interp)
# -namespace is the Tcl namespace look for the implementations under
#            (default is global)
# -uri    the XML namespace for these methods. Defaults to the Tcl interpreter
#         and namespace name.
#
proc ::SOAP::Domain::register {args} {
    # Sanity checks:
    if { [llength $args] < 1 } {
        return -code error "invalid # args:\
              should be \"register ?option value  ...?\""
    }

    # set the default options. These work out to be the current interpreter,
    # toplevel namespace and under /soap URL
    array set opts [list \
            -prefix /soap \
            -namespace {::} \
            -interp {} \
            -dir . \
            -formhandler ::SOAP::Domain::echohandler \
            -uri {^} ]

    # process the arguments
    foreach {opt value} $args {
        switch -glob -- $opt {
            -pre* {set opts(-prefix) $value}
            -nam* {set opts(-namespace) ::$value}
            -int* {set opts(-interp) $value}
            -uri  {set opts(-uri) $value}
            -dir  {set opts(-dir) $value}
            -form* {set opts(-formhandler) $value}
            default {
                set names [join [array names opts -*] ", "]
                return -code error "unrecognised option \"$opt\":\
                       must be one of $names"
            }
        }
    }

    # Construct a URI if not supplied (as indicated by the funny character)
    # gives interpname hyphen namespace path (with more hyphens)
    if { $opts(-uri) == {^} } {
        set opts(-uri) 
        regsub -all -- {::+} "$opts(-interp)::$opts(-namespace)" {-} r
        set opts(-uri) [string trim $r -]
    }

    # Generate the fully qualified name of our options array variable.
    set optname [namespace current]::opts$opts(-prefix)

    # check we didn't already have this registered.
    if { [info exists $optname] } {
        return -code error "URL prefix \"$opts(-prefix)\" already registered"
    }

    # set up the URL domain handler procedure.
    # As interp eval {} evaluates in the current interpreter we can define
    # both a slave interpreter _and_ a specific namespace if we need.

    # If required create a slave interpreter.
    if { $opts(-interp) != {} } {
        catch {interp create -- $opts(-interp)}
    }
    
    # Now create a command in the slave interpreter's target namespace that
    # links to our implementation in this interpreter in the SOAP::Domain
    # namespace.
    interp alias $opts(-interp) $opts(-namespace)::URLhandler \
            {} [namespace current]::domain_handler $optname

    # Register the URL handler with tclhttpd now.
    Url_PrefixInstall $opts(-prefix) \
        [list interp eval $opts(-interp) $opts(-namespace)::URLhandler]

    # log the uri/domain registration
    array set [namespace current]::opts$opts(-prefix) [array get opts]

    return $opts(-prefix)
}

# -------------------------------------------------------------------------

# SOAP URL Domain handler
#
# Called from the namespace or interpreter domain_handler to perform the
# work.
# optsname the qualified name of the options array set up during registration.
# sock     socket back to the client
# suffix   the remainder of the url once the prefix was stripped.
#
proc ::SOAP::Domain::domain_handler {optsname sock args} {
    variable debug
    upvar \#0 Httpd$sock data
    upvar \#0 $optsname options

    # if suffix is {} then it fails to make it through the various evals.
    set suffix [lindex $args 0]
    
    # check this is an XML post
    set failed [catch {set type $data(mime,content-type)} msg]
    if { $failed } {
        set msg "Invalid SOAP request: not XML data"
        log::log debug $msg
        Httpd_ReturnData $sock text/html [html_fault SOAP-ENV:Client $msg] 500
        return $failed
    }

    # make sure we were sent some XML
    set failed [catch {set query $data(query)} msg]
    if { $failed } {
        set msg "Invalid SOAP request: no data sent"
        log::log debug $msg
        Httpd_ReturnData $sock text/html [html_fault SOAP-ENV:Client $msg] 500
        return $failed
    }

    # Check that we have a properly registered domain
    if { ! [info exists options] } {
        set msg "Internal server error: domain improperly registered"
        log::log debug $msg
        Httpd_ReturnData $sock text/html [html_fault SOAP-ENV:Server $msg] 500
        return 1
    }        

    set mime {}
    set doc  {}
    switch -glob -- $type {
        application/x-www-urlencoded --
        application/x-www-form-urlencoded {
            return [htmlform_handler $optsname $sock]
        }
        multipart/related* {
            package require mime
            set mime [mime::initialize -string "Content-type: $type\n\n$query"]
            set parts [mime::getproperty $mime parts]
            set sp [lindex $parts 0]
            if {[llength $parts] > 0 \
                    && [mime::getproperty $sp content] == "text/xml"} {
                set doc [dom::DOMImplementation parse [mime::getbody $sp]]
            }            
        }
        text/xml* {
            # Parse the XML into a DOM tree.
            set doc [dom::DOMImplementation parse $query]
        }
        default {
            set msg "Invalid request: MIME type \"$type\"is not applicable."
            log::log debug $msg
            Httpd_ReturnData $sock text/html \
                [html_fault SOAP-ENV:Client $msg] 500
            return 1
        }
    }

    if { $debug } { set ::doc $doc }

    # Identify the protocol of the request - SOAP or XML-RPC - and identify
    # the name of the method to call.

    if {[selectNode $doc "/Envelope"] != {}} {
        # This is a SOAP protocol call:
        set protocol soap
        if {[catch {
            set methodNodes [selectNode $doc "/Envelope/Body/*"]
            set methodNode [lindex $methodNodes 0]
            set cmdName [nodeName $methodNode]
            set methodName "$options(-namespace)::$cmdName"
        } msg]} {
            Httpd_ReturnData $sock \
			     text/html \
			     [html_fault SOAP-ENV:Client $msg] \
			     500
            catch {dom::DOMImplementation destroy $doc}
            if {$mime != {}} { mime::finalize $mime -subordinates all }
            return 1
        }
    } elseif {[selectNode $doc "/methodCall"] != {}} {
        # This is an XML-RPC protocol call:
        set protocol xmlrpc
        if {[catch {
            set methodNode [selectNode $doc "/methodCall/methodName"]
            set cmdName [getElementValue $methodNode]
            set methodName "$options(-namespace)::$cmdName"
        } msg]} {
            Httpd_ReturnData $sock \
			     text/html \
			     [html_fault SOAP-ENV:Server $msg] \
			     500
            catch {dom::DOMImplementation destroy $doc}
            if {$mime != {}} { mime::finalize $mime -subordinates all }
            return 1
        }
    } else {
        # We didn't understand that.
        set msg "Invalid webservice protocol. \
                 The request was neither a SOAP call nor an XML-RPC call."
        Httpd_ReturnData $sock \
			 text/html \
                         [html_fault SOAP-ENV:Server $msg] \
                         500
        catch {dom::DOMImplementation destroy $doc}
        if {$mime != {}} { mime::finalize $mime -subordinates all }
        return 1
    }

    # Ensure the method is loaded, and bail out otherwise:

    if {![ensureLoaded $methodName \
		       $cmdName \
                       $options(-namespace) \
            	       $options(-dir) \
                       $options(-interp)]} {
        set msg "Method $methodName not found"
        Httpd_ReturnData $sock \
			 text/html \
			 [html_fault SOAP-ENV:Server $msg] \
			 500
        catch {dom::DOMImplementation destroy $doc}
        if {$mime != {}} { mime::finalize $mime -subordinates all }
        return 1
    }

    # Call the method according to the protocol selected by the user:

    if {$protocol == "soap"} {
        # Call the SOAP procedure and convert errors into SOAP Faults and
        # the return data into a SOAP return packet.
        set failed [catch {
            SOAP::CGI::soap_call $doc $mime \
                $options(-interp) $options(-namespace)
        } msg]
        Httpd_ReturnData $sock text/xml $msg [expr {$failed ? 500 : 200}]
        catch {dom::DOMImplementation destroy $doc}
        if {$mime != {}} {mime::finalize $mime -subordinates all}
        return $failed
    }

    # Call the XML-RPC procedure.
    set failed [catch {
        SOAP::CGI::xmlrpc_call $doc \
            $options(-interp) $options(-namespace)
    } msg]

    #  Note that the XML-RPC spec says one should always return 200 unless
    #  there's a low-level server error; in particular an XML-RPC fault
    #  return should be accompanied by a 200 response code.
    Httpd_ReturnData $sock text/xml $msg 200
    catch {dom::DOMImplementation destroy $doc}
    if {$mime != {}} {mime::finalize $mime -subordinates all}
    return $failed
}

# Ensure the requested method is loaded into the target interpreter:
#
# 1. Check if the method is already defined -- if so, return true.
# 2. Ensure the directory is part of the auto_path and package require
#    the namespace (use the namespace as a package name). See if it's now
#    loaded, if so, return true.
# 3. Split the command name (usually something like weblogUpdates.ping) on
#    the '.', use [file join $dir <first-part>.tcl] as a file name, and source
#    that. If the method is now defined, return true.
# 4. Give up and return false.

proc SOAP::Domain::ensureLoaded {methodName cmdName namespace dir interp} {
    catch {
        # Check if it's already loaded:
        if {[interp eval $interp [list info commands $methodName]] != ""} {
            return true
        }

        # Ensure that the dir is part of the auto-path in the interp,
        # and then force a package require in that interp, using the
        # namespace as the package name.
        if {[interp eval $interp \
                 [list uplevel \#0 [list lsearch \$auto_path $dir]]]
            == -1} {
            interp eval $interp \
                [list uplevel \#0 [list lappend auto_path $dir]]
        }
        catch {interp eval $interp \
                   [list uplevel \#0 [list package require $namespace]]}

        # If the method is  now loaded, return true:
        if {[interp eval $interp [list info commands $methodName]] != ""} {
            return true
        }

        # Try to split the cmdName on "." and use the first part
        # as the name of a file to source into the interp:
        set fileName [file join $dir [lindex [split $cmdName "."] 0].tcl]
        if {[catch {interp eval $interp \
                        [list uplevel \#0 [list source $fileName]]}]} {
            return false
        }
        if {[interp eval $interp [list info commands $methodName]] != ""} {
            return true
        }

        # Give up and return false:
        return false
    } msg
    return $msg
}

# Special handler for HTML forms. Parse the query part and call
# the implementation directly.

proc ::SOAP::Domain::htmlform_handler {optsname sock} {
    upvar \#0 Httpd$sock data
    upvar \#0 $optsname options

    # Decode the query:

    set query [::ncgi::decode $data(query)]

    # Split it apart:

    set nvlist {}
    foreach {x} [split [string trim $query] &] {
        # Turns out you might not get an = sign,
        # especially with <isindex> forms.
        if {![regexp -- (.*)=(.*) $x dummy varname val]} {
            set varname anonymous
            set val $x
        }
        lappend nvlist $varname $val
    }

    # If the handler is the default handler, then run it in
    # the current interp and be done:

    if {$options(-formhandler) == "SOAP::Domain::echohandler"} {
        return [SOAP::Domain::echohandler $sock $nvlist]
    }

    # Identify the method to call:

    set cmdName $options(-formhandler)
    set methodName $options(-namespace)::$cmdName

    # Ensure the method to call is loaded, and bail out otherwise:

    if {![ensureLoaded $methodName \
		       $cmdName \
                       $options(-namespace) \
            	       $options(-dir) \
                       $options(-interp)]} {
        set msg "Method $methodName not found"
        Httpd_ReturnData $sock \
			 text/html \
			 [html_fault SOAP-ENV:Server $msg] \
			 500
        return 1
    }

    # Issue the call:

    if {[catch {interp eval $options(-interp) \
                    $methodName $sock [list $nvlist]} msg]} {

        # Convention: If success, the method already called Httpd_ReturnData
        # so we only do it for errors. Also, the mime type should be text/html.

        Httpd_ReturnData $sock \
			 text/html \
			 [html_fault SOAP-ENV:Server $msg] \
			 500

        return 1
    }

    return 0
}

# Default handler for HTML forms:

proc ::SOAP::Domain::echohandler {sock nvlist} {
    set type text/html
    set html {}
    append html "<html>\n<head>\n<title>Form sent to SOAP"
    append html "</title>\n</head>\n<body>\n"
    append html "<h2>Form type: $type</h2><ul>"

    foreach {name value} $nvlist {
        append html "<li>$name&nbsp;$value</li>"
    }

    append html "</ul></body></html>"

    Httpd_ReturnData $sock $type $html 200

    # Indicate success:

    return 0
}

# Return an html wrapping of an error:

proc ::SOAP::Domain::html_fault {type msg} {
    set html "<html><head>\
        <title>Webservice Error</title></head><body>\
        <h2>$msg</h2>\
        </body></html>"
    return $html
}

# -------------------------------------------------------------------------

package provide SOAP::Domain $::SOAP::Domain::version

# -------------------------------------------------------------------------

# Local variables:
#   mode: tcl
#   indent-tabs-mode: nil
# End:

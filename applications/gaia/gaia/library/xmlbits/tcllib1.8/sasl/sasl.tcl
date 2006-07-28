# sasl.tcl - Copyright (C) 2005 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# This is an implementation of a general purpose SASL library for use in
# Tcl scripts. 
#
# References:
#    Myers, J., "Simple Authentication and Security Layer (SASL)", 
#      RFC 2222, October 1997.
#    Rose, M.T., "TclSASL", "http://beepcore-tcl.sourceforge.net/tclsasl.html"
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------
package require Tcl 8.2

namespace eval ::SASL {
    variable version 1.0.0
    variable rcsid {$Id$}

    variable uid
    if {![info exists uid]} { set uid 0 }

    variable mechanisms
    if {![info exists mechanisms]} {
        set mechanisms [list]
    }
}

# -------------------------------------------------------------------------

proc ::SASL::mechanisms {} {
    variable mechanisms
    set r [list]
    foreach mech $mechanisms {
        lappend r [lindex $mech 1]
    }
    return $r
}

proc ::SASL::register {mechanism preference clientproc {serverproc {}}} {
    variable mechanisms
    lappend mechanisms [list $preference $mechanism $clientproc $serverproc]
    set mechanisms [lsort -index 0 -decreasing -integer $mechanisms]
    return
}

proc ::SASL::uid {} {
    variable uid
    return [incr uid]
}

proc ::SASL::response {context} {
    upvar #0 $context ctx
    return $ctx(response)
}

proc ::SASL::reset {context} {
    upvar #0 $context ctx
    array set ctx [list step 0 response "" valid false]
    return $context
}

proc ::SASL::cleanup {context} {
    if {[info exists $context]} {
        unset $context
    }
    return
}

# sasl::client_new --
#
#	Create a new client connection context.
#
proc ::SASL::new {args} {
    set context [namespace current]::[uid]
    variable $context
    upvar #0 $context ctx
    array set ctx [list mech {} callback {} proc {} service {} server {} \
                       step 0 response "" valid false type client]
    eval [linsert $args 0 [namespace origin configure] $context]
    return $context
}

proc ::SASL::configure {context args} {
    variable mechanisms
    upvar #0 $context ctx
    while {[string match -* [set option [lindex $args 0]]]} {
        switch -exact -- $option {
            -service {
                set ctx(service) [Pop args 1]
            }
            -server - -serverFQDN {
                set ctx(server) [Pop args 1]
            }
            -mech - -mechanism {
                set mech [string toupper [Pop args 1]]
                set ctx(proc) {}
                foreach m $mechanisms {
                    if {[string equal [lindex $m 1] $mech]} {
                        set ctx(mech) $mech
                        if {[string equal $ctx(type) "server"]} {
                            set ctx(proc) [lindex $m 3]
                        } else {
                            set ctx(proc) [lindex $m 2]
                        }
                        break
                    }
                }
                if {[string equal $ctx(proc) {}]} {
                    return -code error "mechanism \"$mech\" not available:\
                        must be one of those given by \[sasl::mechanisms\]"
                }
            }
            -callback - -callbacks {
                set ctx(callback) [Pop args 1]
            }
            -type {
                set type [Pop args 1]
                if {[lsearch -exact {server client} $type] != -1} {
                    set ctx(type) $type
                    if {![string equal $ctx(mech) ""]} {
                        configure $context -mechanism $ctx(mech)
                    }
                } else {
                    return -code error "bad value \"$type\":\
                        must be either client or server"
                }
            }
            default {
                return -code error "bad option \"$option\":\
                    must be one of -mechanism, -service, -server -type\
                    or -callbacks"
            }
        }
        Pop args
    }
        
}

proc ::SASL::step {context challenge args} {
    upvar #0 $context ctx
    return [eval [linsert $args 0 $ctx(proc) $context $challenge]]
}


proc ::SASL::Pop {varname {nth 0}} {
    upvar $varname args
    set r [lindex $args $nth]
    set args [lreplace $args $nth $nth]
    return $r
}

proc ::SASL::md5_init {} {
    variable md5_inited
    if {[info exists md5_inited]} {return} else {set md5_inited 1}
    # Deal with either version of md5. We'd like version 2 but someone
    # may have already loaded version 1.
    set md5major [lindex [split [package require md5] .] 0]
    if {$md5major < 2} {
        # md5 v1, no options, and returns a hex string ready for us.
        proc ::SASL::md5_hex {data} { return [::md5::md5 $data] }
        proc ::SASL::md5_bin {data} { return [binary format H* [::md5::md5 $data]] }
        proc ::SASL::hmac_hex {pass data} { return [::md5::hmac $pass $data] }
        proc ::SASL::hmac_bin {pass data} { return [binary format H* [::md5::hmac $pass $data]] }
    } else {
        # md5 v2 requires -hex to return hash as hex-encoded non-binary string.
        proc ::SASL::md5_hex {data} { return [string tolower [::md5::md5 -hex $data]] }
        proc ::SASL::md5_bin {data} { return [::md5::md5 $data] }
        proc ::SASL::hmac_hex {pass data} { return [::md5::hmac -hex -key $pass $data] }
        proc ::SASL::hmac_bin {pass data} { return [::md5::hmac -key $pass $data] }
    }
}

# -------------------------------------------------------------------------

# CRAM-MD5 SASL MECHANISM
#
# 	Implementation of the Challenge-Response Authentication Mechanism
#	(RFC2195).
#
# Comments:
#	This mechanism passes a server generated string containing
#	a timestamp and has the client generate an MD5 HMAC using the
#	shared secret as the key and the server string as the data.
#	The downside of this protocol is that the server must have access
#	to the plaintext password.
#
proc ::SASL::CRAM-MD5:client {context challenge args} {
    upvar #0 $context ctx
    md5_init
    if {$ctx(step) != 0} {
        return -code error "unexpected state: CRAM-MD5 has only 1 step"
    }
    set password [eval $ctx(callback) [list $context password]]
    set username [eval $ctx(callback) [list $context username]]
    set reply [hmac_hex $password $challenge]
    set reply "$username [string tolower $reply]"
    set ctx(response) $reply
    incr ctx(step)
    return 0
}

proc ::SASL::CRAM-MD5:server {context clientrsp args} {
    upvar #0 $context ctx
    md5_init
    incr ctx(step)
    switch -exact -- $ctx(step) {
        1 {
            set ctx(realm) [eval $ctx(callback) [list $context realm]]
            set ctx(response) "<[pid].[clock seconds]@$ctx(realm)>"
            return 1
        }
        2 {
            foreach {user hash} $clientrsp break
            set hash [string tolower $hash]
            set pass [eval $ctx(callback) [list $context password $user $ctx(realm)]]
            set check [hmac_bin $pass $ctx(response)]
            binary scan $check H* cx
            if {[string equal $cx $hash]} {
                return 0
            } else {
                return -code error "authentication failed"
            }
        }
        default {
            return -code error "invalid state"
        }
    }
}

::SASL::register CRAM-MD5 30 ::SASL::CRAM-MD5:client ::SASL::CRAM-MD5:server

# -------------------------------------------------------------------------
# PLAIN SASL MECHANISM
#
# 	Implementation of the single step login SASL mechanism (RFC2595).
#
# Comments:
#	A single step mechanism in which the authorization ID, the
#	authentication ID and password are all transmitted in plain
#	text. This should not be used unless the channel is secured by
#	some other means (such as SSL/TLS).
#
proc ::SASL::PLAIN:client {context challenge args} {
    upvar #0 $context ctx
    incr ctx(step)
    set authzid  [eval $ctx(callback) [list $context login]]
    set username [eval $ctx(callback) [list $context username]]
    set password [eval $ctx(callback) [list $context password]]
    set ctx(response) "$authzid\x00$username\x00$password"
    return 0
}

proc ::SASL::PLAIN:server {context clientrsp args} {
    upvar \#0 $context ctx
    if {[string length $clientrsp] < 1} {
        set ctx(response) ""
        return 1
    } else {
        foreach {authzid authid pass} [split $clientrsp \0] break
        set check  [eval $ctx(callback) [list $context password $authid]]
        if {[string equal $pass $check]} {
            return 0
        } else {
            return -code error "authentication failed"
        }
    }
}

::SASL::register PLAIN 10 ::SASL::PLAIN:client ::SASL::PLAIN:server

# -------------------------------------------------------------------------
# LOGIN SASL MECHANISM
#
# 	Implementation of the two step login SASL mechanism.
#
# Comments:
#	This is an unofficial but widely deployed SASL mechanism somewhat
#	akin to the PLAIN mechanism. Both the authentication ID and password
#	are transmitted in plain text in response to server prompts.
#
#	NOT RECOMMENDED for use in new protocol implementations.
#
proc ::SASL::LOGIN:client {context challenge args} {
    upvar #0 $context ctx
    incr ctx(step)
    switch -exact -- $ctx(step) {
        1 {
            set ctx(response) [eval $ctx(callback) [list $context username]]
            set r 1
        }
        2 {
            set ctx(response) [eval $ctx(callback) [list $context password]]
            set r 0
        }
        default {
            return -code error "unexpected state \"$ctx(step)\":\
                LOGIN has only 2 steps"
        }
    }
    return $r
}

proc ::SASL::LOGIN:server {context clientrsp args} {
    upvar #0 $context ctx
    incr ctx(step)
    switch -exact -- $ctx(step) {
        1 {
            set ctx(response) "Username:"
            return 1
        }
        2 {
            set ctx(username) $clientrsp
            set ctx(response) "Password:"
            return 1
        }
        3 {
            set pass [eval $ctx(callback) [list $context password $user $ctx(realm)]]
            if {[string equal $clientrsp $pass]} {
                return 0
            } else {
                return -code error "authentication failed"
            }
        }
        default {
            return -code error "invalid state"
        }
    }
}

::SASL::register LOGIN 20 ::SASL::LOGIN:client ::SASL::LOGIN:server

# -------------------------------------------------------------------------
# ANONYMOUS SASL MECHANISM
#
# 	Implementation of the ANONYMOUS SASL mechanism (RFC2245).
#
# Comments:
#
# 
proc ::SASL::ANONYMOUS:client {context challenge args} {
    upvar #0 $context ctx
    set ctx(response) [lindex $args 0]
    return 0
}

proc ::SASL::ANONYMOUS:server {context clientrsp args} {
    upvar #0 $context ctx
    set ctx(response) ""
    if {[string length $clientrsp] < 1} {
        return 1
    } else {
        set ctx(trace) $clientrsp
        return 0
    }
}

::SASL::register ANONYMOUS 5 ::SASL::ANONYMOUS:client ::SASL::ANONYMOUS:server

# -------------------------------------------------------------------------

# DIGEST-MD5 SASL MECHANISM
#
# 	Implementation of the DIGEST-MD5 SASL mechanism (RFC2831).
#
# Comments:
#
proc ::SASL::DIGEST-MD5:client {context challenge args} {
    variable digest_md5_noncecount
    upvar #0 $context ctx
    md5_init
    incr ctx(step)
    set result 0
    switch -exact -- $ctx(step) {
        1 {
            # RFC 2831 2.1
            # Char categories as per spec...
            # Build up a regexp for splitting the challenge into key value pairs.
            set sep "\\\]\\\[\\\\()<>@,;:\\\"\\\?=\\\{\\\} \t"
            set tok {0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\-\|\~\!\#\$\%\&\*\+\.\^\_\`}
            set sqot {(?:\'(?:\\.|[^\'\\])*\')}
            set dqot {(?:\"(?:\\.|[^\"\\])*\")}
            set parameters {}
            regsub -all "(\[${tok}\]+)=(${dqot}|(?:\[${tok}\]+))(?:\[${sep}\]+|$)" $challenge {\1 \2 } parameters
            array set params $parameters
            
            if {![info exists digest_md5_noncecount]} {
                set digest_md5_noncecount 0
            }
            set nonce $params(nonce)
            set cnonce [CreateNonce]
            set noncecount [format %08u [incr digest_md5_noncecount]]
            set qop auth
            
            set username [eval $ctx(callback) [list $context username]]
            set password [eval $ctx(callback) [list $context password]]
            if {[info exists params(realm)]} {
                set realm $params(realm)
            } else {
                set realm [eval $ctx(callback) [list $context realm]]
            }
            
            set uri "smtp/$realm"
            
            set A1 [md5_bin "$username:$realm:$password"]
            set A2 "AUTHENTICATE:$uri"
            if {![string equal $qop "auth"]} {
                append A2 :[string repeat 0 32]
            }
            
            set A1h [md5_hex "${A1}:$nonce:$cnonce"]
            set A2h [md5_hex $A2]
            set R   [md5_hex $A1h:$nonce:$noncecount:$cnonce:$qop:$A2h]
            
            set ctx(response) "username=\"$username\",realm=\"$realm\",nonce=\"$nonce\",nc=\"$noncecount\",cnonce=\"$cnonce\",digest-uri=\"$uri\",response=\"$R\",qop=$qop"
            set result 1
        }
        
        2 {
            set ctx(response) ""
            set result 0
        }
        default {
            return -code error "invalid state"
        }
    }
    return $result
}

# Get 16 random bytes for a nonce value. If we can use /dev/random, do so
# otherwise we hash some values.
#
proc ::SASL::CreateNonce {} {
    set bytes {}
    if {[file readable /dev/random]} {
        catch {
            set f [open /dev/random r]
            fconfigure $f -translation binary -buffering none
            set bytes [read $f 16]
            close $f
        }
    }
    if {[string length $bytes] < 1} {
        set bytes [md5_bin [clock seconds]:[pid]:[expr {rand()}]]
    }
    return [binary scan $bytes h* r; set r]
}

::SASL::register DIGEST-MD5 40 ::SASL::DIGEST-MD5:client

# -------------------------------------------------------------------------
#
# Local variables:
# indent-tabs-mode: nil
# End:
# -------------------------------------------------------------------------

package provide SASL $::SASL::version

# -------------------------------------------------------------------------
#
# Local variables:
#   indent-tabs-mode: nil
# End:

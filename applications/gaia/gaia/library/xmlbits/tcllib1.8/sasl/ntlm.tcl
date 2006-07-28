# ntlm.tcl - Copyright (C) 2005 Pat Thoyts <patthoyts@users.sourceforge.net>
#
# This is an implementation of Microsoft's NTLM authentication mechanism.
#
# References:
#    http://www.innovation.ch/java/ntlm.html
#    http://davenport.sourceforge.net/ntlm.html
#
# -------------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# -------------------------------------------------------------------------

package require Tcl 8.2;                # tcl minimum version
package require SASL 1.0;               # tcllib 1.7
package require des 1.0;                # tcllib 1.8
package require md4;                    # tcllib 1.4

#package require log;                   # tcllib 1.4
#log::lvSuppressLE emerg 0

namespace eval ::SASL {
    namespace eval NTLM {
        variable version 1.0.0
        variable rcsid {$Id$}
    }
}

# -------------------------------------------------------------------------

proc ::SASL::NTLM::NTLM {context challenge args} {
    upvar #0 $context ctx
    incr ctx(step)
    switch -exact -- $ctx(step) {
        
        1 {
            set ctx(realm) [eval [linsert $ctx(callback) end $context realm]]
            set ctx(hostname) [eval [linsert $ctx(callback) end $context hostname]]
            set ctx(response)   [CreateGreeting $ctx(realm) $ctx(hostname)]
            set result 1
        }

        2 {
            array set params [Decode $challenge]
            set user [eval [linsert $ctx(callback) end $context username]]
            set pass [eval [linsert $ctx(callback) end $context password]]
            set ctx(response) [CreateResponse \
                                   $ctx(realm) $ctx(hostname) \
                                   $user $pass $params(nonce)]
            Decode $ctx(response)
            set result 0
        }
        default {
            return -code error "invalid state \"$ctx(step)"
        }
    }
    return $result
}

# -------------------------------------------------------------------------
# NTLM client implementation
# -------------------------------------------------------------------------

# The NMLM greeting. This is sent by the client to the server to initiate
# the challenge response handshake.
# This message contains the hostname (not domain qualified) and the 
# NT domain name for authentication.
#
proc ::SASL::NTLM::CreateGreeting {domainname hostname} {
    set domain [encoding convertto ascii $domainname]
    set host [encoding convertto ascii $hostname]
    set d_len [string length $domain]
    set h_len [string length $host]
    set d_off [expr {32 + $h_len}]
    set msg [binary format a8iississi \
                 "NTLMSSP\x00" 1 [expr {0x3207}] \
                 $d_len $d_len $d_off \
                 $h_len $h_len 32]
    append msg $host $domain
    return $msg
}

# Create a NTLM server challenge. This is sent by a server in response to
# a client type 1 message. The content of the type 2 message is variable
# and depends upon the flags set by the client and server choices.
#
proc ::SASL::NTLM::CreateChallenge {domainname} {
    SASL::md5_init
    set target  [encoding convertto ascii $domainname]
    set t_len   [string length $target]
    set nonce   [string range [binary format h* [SASL::CreateNonce]] 0 7]
    set pad     [string repeat \0 8]
    set context [string repeat \0 8]
    set msg [binary format a8issii \
                 "NTLMSSP\x00" 2 \
                 $t_len $t_len 48 \
                 [expr {0x01028100}]]
    append msg $nonce $pad $context $pad $target
    return $msg
}

# Compose the final client response. This contains the encoded username
# and password, along with the server nonce value.
#
proc ::SASL::NTLM::CreateResponse {domainname hostname username passwd nonce} {
    set lm_resp [LMhash $passwd $nonce]
    set nt_resp [NThash $passwd $nonce]

    set domain [encoding convertto unicode [string toupper $domainname]]
    set host   [encoding convertto unicode [string toupper $hostname]]
    set user   [encoding convertto unicode $username]

    set l_len [string length $lm_resp]; # LM response length
    set n_len [string length $nt_resp]; # NT response length
    set d_len [string length $domain];  # Domain name length
    set h_len [string length $host];    # Host name length
    set u_len [string length $user];    # User name length

    # The full message length
    set m_len [expr {0x40 + $d_len + $u_len + $h_len + $n_len + $l_len}]

    # The offsets to strings appended to the structure
    set l_off [expr {0x40 + $d_len + $u_len + $h_len}]
    set n_off [expr {0x40 + $d_len + $u_len + $h_len + $l_len}]
    set d_off [expr {0x40}]
    set u_off [expr {0x40 + $d_len}]
    set h_off [expr {0x40 + $d_len + $u_len}]

    set msg [binary format a8is4s4s4s4s4iii \
                 "NTLMSSP\x00" 3 \
                 [list $l_len $l_len $l_off 0] \
                 [list $n_len $n_len $n_off 0] \
                 [list $d_len $d_len $d_off 0] \
                 [list $u_len $u_len $u_off 0] \
                 [list $h_len $h_len $h_off 0] \
                 $m_len 0x0201 0]
    append msg $domain $user $host $lm_resp $nt_resp

    return $msg
}

proc ::SASL::NTLM::Debug {msg} {
    array set d [Decode $msg]
    if {[info exists d(flags)]}  { set d(flags) [list [format 0x%08x $d(flags)] [decodeflags $d(flags)]] }
    if {[info exists d(nonce)]}  { set d(nonce) [base64::encode $d(nonce)] }
    if {[info exists d(lmhash)]} { set d(lmhash) [base64::encode $d(lmhash)] }
    if {[info exists d(nthash)]} { set d(nthash) [base64::encode $d(nthash)] }
    return [array get d]
}

proc ::SASL::NTLM::Decode {msg} {
    binary scan $msg a7ci protocol zero type

    switch -exact -- $type {
        1 {
            binary scan $msg @12ississi flags dlen dlen2 doff hlen hlen2 hoff
            binary scan $msg @${hoff}a${hlen} host
            binary scan $msg @${doff}a${dlen} domain
            #log::log debug "NTLM($type) [decodeflags $flags]\n \
            #    '$host' '$domain'"
            return [list type $type flags [format 0x%08x $flags] domain $domain host $host]
        }
        2 {
            binary scan $msg @12ssiia8a8 dlen dlen2 doff flags nonce pad
            set domain {}; binary scan $msg @${doff}a${dlen} domain
            set domain [encoding convertfrom unicode $domain]
            binary scan $nonce H* nonce_h
            binary scan $pad   H* pad_h
            #puts stderr "NTLM($type) [decodeflags $flags]\n \
            #    '$domain' '$nonce_h' '$pad_h'"
            return [list type $type flags [format 0x%08x $flags] domain $domain nonce $nonce]
        }
        3 {
            binary scan $msg @12ssississississiii \
                lmlen lmlen2 lmoff ntlen ntlen2 ntoff \
                dlen  dlen2  doff  ulen  ulen2  uoff \
                hlen  hlen2  hoff \
                mlen  flags
            set domain {}; binary scan $msg @${doff}a${dlen} domain
            set user {};   binary scan $msg @${uoff}a${ulen} user
            set host {};   binary scan $msg @${hoff}a${hlen} host
            set domain [encoding convertfrom unicode $domain]
            set user   [encoding convertfrom unicode $user]
            set host   [encoding convertfrom unicode $host]
            binary scan $msg @${ntoff}a${ntlen} ntdata
            binary scan $msg @${lmoff}a${lmlen} lmdata
            binary scan $ntdata H* ntdata_h
            binary scan $lmdata H* lmdata_h
            #log::log debug "NTLM($type) [decodeflags $flags]\n \
            #    mlen:$mlen '$domain' '$host' '$user'"
            #log::log debug "  LM '$lmdata_h'\n  NT '$ntdata_h'"
            return [list type $type flags [format 0x%08x $flags] domain $domain \
                        host $host user $user lmhash $lmdata nthash $ntdata]
        }
    }
}

proc ::SASL::NTLM::decodeflags {value} {
    set flags {
        0x0001 unicode 0x0002 oem    0x0004 req_target 0x0008 unknown 
        0x0010 sign    0x0020 seal   0x0040 datagram   0x0080 lmkey 
        0x0100 netware 0x0200 ntlm   0x0400 unknown    0x0800 unknown
        0x1000 domain  0x2000 server 0x4000 share      0x8000 NTLM2
        0x00800000 targetinfo 0x20000000 128bit 0x40000000 keyexch
        0x80000000 56bit
    }
    set r {}
    foreach {mask name} $flags {
        if {$value & ($mask & 0xffffffff)} {
            lappend r $name
        }
    }
    return $r
}

proc ::SASL::NTLM::LMhash {password nonce} {
    set magic "\x4b\x47\x53\x21\x40\x23\x24\x25"
    set hash ""
    set password [string range [string toupper $password][string repeat \0 14] 0 13]
    foreach key [CreateDesKeys $password] {
        append hash [DES::des -dir encrypt -weak -mode ecb -key $key $magic]
    }

    append hash [string repeat \0 5]
    set res ""
    foreach key [CreateDesKeys $hash] {
        append res [DES::des -dir encrypt -weak -mode ecb -key $key $nonce]
    }

    return $res
}

proc ::SASL::NTLM::NThash {password nonce} {
    set pass [encoding convertto unicode $password]
    set hash [md4::md4 $pass]
    append hash [string repeat \x00 5]

    set res ""
    foreach key [CreateDesKeys $hash] {
        append res [DES::des -dir encrypt -weak -mode ecb -key $key $nonce]
    }

    return $res
}

# Convert a password into a 56 bit DES key according to the NTLM specs.
# We do NOT fix the parity of each byte. If we did, then bit 0 of each
# byte should be adjusted to give the byte odd parity.
#
proc ::SASL::NTLM::CreateDesKeys {key} {
    # pad to 7 byte boundary with nuls.
    set mod [expr {[string length $key] % 7}]
    if {$mod != 0} {
        append key [string repeat "\0" [expr {7 - $mod}]]
    }
    set len [string length $key]
    set r ""
    for {set n 0} {$n < $len} {incr n 7} {
        binary scan $key @${n}c7 bytes
        set b {}
        lappend b [expr {  [lindex $bytes 0] & 0xFF}]
        lappend b [expr {(([lindex $bytes 0] & 0x01) << 7) | (([lindex $bytes 1] >> 1) & 0x7F)}]
        lappend b [expr {(([lindex $bytes 1] & 0x03) << 6) | (([lindex $bytes 2] >> 2) & 0x3F)}]
        lappend b [expr {(([lindex $bytes 2] & 0x07) << 5) | (([lindex $bytes 3] >> 3) & 0x1F)}]
        lappend b [expr {(([lindex $bytes 3] & 0x0F) << 4) | (([lindex $bytes 4] >> 4) & 0x0F)}]
        lappend b [expr {(([lindex $bytes 4] & 0x1F) << 3) | (([lindex $bytes 5] >> 5) & 0x07)}]
        lappend b [expr {(([lindex $bytes 5] & 0x3F) << 2) | (([lindex $bytes 6] >> 6) & 0x03)}]
        lappend b [expr {(([lindex $bytes 6] & 0x7F) << 1)}]
        lappend r [binary format c* $b]
    }
    return $r;
}

# This is slower than the above in Tcl 8.4.9
proc ::SASL::NTLM::CreateDesKeys2 {key} {
    # pad to 7 byte boundary with nuls.
    append key [string repeat "\0" [expr {7 - ([string length $key] % 7)}]]
    binary scan $key B* bin
    set len [string length $bin]
    set r ""
    for {set n 0} {$n < $len} {incr n} {
        append r [string range $bin $n [incr n  6]] 0
    }
    # needs spliting into 8 byte keys.
    return [binary format B* $r]
}

# -------------------------------------------------------------------------

# Register this SASL mechanism with the Tcllib SASL package.
#
if {[llength [package provide SASL]] != 0} {
    ::SASL::register NTLM 50 ::SASL::NTLM::NTLM
}

package provide SASL::NTLM $::SASL::NTLM::version

# -------------------------------------------------------------------------
#
# Local variables:
# indent-tabs-mode: nil
# End:

# digest.tcl
#
# Provides Digest Authentication, per http://www.ietf.org/rfc/rfc2617.txt
# Works in conjunction with auth.tcl, requires tcllib.
# Digest authentication is selectable in .htaccess.
# No provision for dual Basic/Digest authentication.
#
# Supports only algorithm=MD5 and qop=auth
# (which is more than most browsers support :)
#
# Copyright 2004 Colin McCormack.  colin@chinix.com
# Licensed on terms identical to tclhttpd's license.

package provide httpd::digest 1.0

package require base64
package require md5

# generate private key
if {[catch {package require Random}]} {
    # use the built in tcl random
    proc DigestRand {} {
	return [expr {rand() * 65536}]
    }
} else {
    # use http://mini.net/tcl/random
    # cvs -d:pserver:anonymous@cvs.tclsoap.sourceforge.net:/cvsroot/tclsoap co Random

    # generate a random seed
    if {[file exists /dev/random]} {
	# try for hardware support
	set r [open /dev/random]
	binary scan [read $r 4] I seed
	close $r
    } else {
	set seed [clock clicks]
    }
    expr {isaac_srand($seed)}	;# seed random

    # get an integer secret
    proc DigestRand {} {
	return [::isaac::isaac integer]
    }
}
set DigestSecret [DigestRand]

# calculate a digest key for a given session
proc DigestA1 {sock} {
    upvar #0 Httpd$sock data
    upvar #0 Digest$data(digest,nonce) digest

    set userstuff "$data(digest,username):$data(digest,realm):$digest(passwd)"
    switch -- [string tolower $data(digest,algorithm)] {
	md5 {
	    set digest(A1) [md5hex $userstuff]
	}
	md5-sess {
	    set digest(A1) [md5hex "[md5hex $userstuff]:$data(digest,nonce):$data(digest,cnonce)"]
	}
	default {
	    error "unknown algorithm: $data(digest,algorithm)"
	}
    }
    return $digest(A1)
}

# per operation hash
proc DigestA2 {sock} {
    upvar #0 Httpd$sock data
    if {[info exists data(op)]} {
	set result [md5hex "[string toupper $data(op)]:$data(digest,uri)"]
	# nb: we don't offer auth-int
    } else {
	set result [md5hex "GET:$data(digest,uri)"]
    }
    return $result
}

# generate a nonce
proc DigestNonce {} {
    #return "dcd98b7102dd2f0e8b11d0f600bfb0c093"	;# test

    global DigestSecret
    set time [clock clicks]
    return [md5hex ${time}:${DigestSecret}]
}

# calculate the digest value for a given operation and session
proc DigestDigest {sock} {
    upvar #0 Httpd$sock data
    upvar #0 Digest$data(digest,nonce) digest
    if {![info exists digest(A1)]} {
	set digest(A1) [DigestA1 $sock]
    }
    set digest(A2) [DigestA2 $sock]

    if {[info exists data(digest,qop)]} {
	set result [md5hex "$digest(A1):$data(digest,nonce):[format %08x 0x$digest(nc)]:$data(digest,cnonce):$data(digest,qop):$digest(A2)"]
    } else {
	set result [md5hex "$digest(A1):$data(digest,nonce):$digest(A2)"]
    }
    return $result
}

# handle the client's Digest
proc Digest_Request {sock realm file} {
    upvar #0 Httpd$sock data

    upvar #0 Digest$data(digest,nonce) digest
    set digest(last) [clock clicks]	;# remember the last use

    # create a digest for this socket
    if {![info exists digest(passwd)]} {
	set digest(passwd) [AuthGetPass $sock $file $data(digest,username)]
    }

    # check that realms match
    if {$realm ne $data(digest,realm)} {
	Stderr "realm"
	return 0
    }

    if {[info exists digest(opaque)]} {
	if {$digest(opaque) ne $data(digest,opaque)} {
	    return 0
	}
    }

    # check the nonce count
    if {[info exists digest(nc)]} {
	if { $data(digest,nc) <= $digest(nc)} {
	    set digest(stale) 1
	    #return 0	;# Mozilla doesn't implement nc
	}
	#incr digest(nc)
	set digest(nc) $data(digest,nc)
    } else {
	return 0	;# this is new to us
    }

    # check the password
    set calc_digest [DigestDigest $sock]
    if {$calc_digest ne $data(digest,response)} {
	return 0
    }

    # successful authentication
    # construct authentication args
    set a_args "qop=auth"
    if {[info exists data(digest,cnonce)]} {
	append a_args ", cnonce=\"$data(digest,cnonce)\""
	append a_args ", rspauth=\"${calc_digest}\""
    }
    if {[info exists data(digest,nc)]} {
	append auth_info [format ", nc=%08x" [expr 1 + 0x$digest(nc)]]
    }

    # remember some data
    set digest(realm) $data(digest,realm)
    set digest(cnonce) $data(digest,cnonce)
    set digest(username) $data(digest,username)

    # associate nonce with realm,user
    global DigestByRealmName
    set DigestByRealmName($digest(realm),$digest(username)) $digest(nonce)

    Httpd_AddHeaders $sock Authentication-Info $auth_info

    return 1
}

# decode an Authentication request
proc Digest_Get {sock parts} {
    upvar #0 Httpd$sock data

    # get the digest request args
    foreach el [lrange $parts 1 end] {
	set el [string trimright $el ,]
	foreach {n v} [split $el =] break
	set data(digest,[string trim $n " "]) [string trim $v { "}]
    }
    
    # perform some desultory checks on credentials
}

# create and issue a Digest challenge to the client
proc Digest_Challenge {sock realm user} {
    upvar #0 Httpd$sock data

    global DigestByRealmName
    if {[info exists DigestByRealmName($realm,$user)]} {
	# find an existing nonce for this realm,user pair
	set nonce $DigestByRealmName($realm,$user)

	upvar #0 Digest$nonce digest
	set digest(last) [clock clicks]	;# remember the last use
    } else {
	# get a new unique nonce
	# (redundant, really MD5 doesn't collide)
	set nonce [DigestNonce]
	while {[info exists ::Digest$nonce]} {
	    set nonce [DigestNonce]
	}
	upvar #0 Digest$nonce digest
	set digest(last) [clock clicks]	;# remember the last use

	# initialise the digest state
	global DigestSecret
	set digest(nonce) $nonce
	set digest(opaque) [md5hex "[clock clicks]${DigestSecret}"]
	#set digest(opaque) 5ccc069c403ebaf9f0171e9517f40e41	;# test
	set digest(nc) 0
	#set digest(stale) 0
    }

    # construct authentication args
    # minimally nonce, opaque, qop and algorithm
    set challenge [list nonce \"$digest(nonce)\" \
		       opaque \"$digest(opaque)\" \
		       qop \"auth\" \
		       algorithm MD5]
#		   algorithm \"MD5,MD5-sess\"]
    #if {$digest(stale)} {
	#lappend challenge stale true
    #}
    
    # issue Digest authentication challenge
    eval Httpd_RequestAuth $sock Digest $realm $challenge
}

if {0} {
    # test
    array set Digest999 {
	realm testrealm@host.com
	qop auth,auth-int
	nonce dcd98b7102dd2f0e8b11d0f600bfb0c093
	opaque 5ccc069c403ebaf9f0171e9517f40e41
	nc 0
    }

    array set Httpd999 {
	digest,username Mufasa
	digest,realm testrealm@host.com
	digest,nonce dcd98b7102dd2f0e8b11d0f600bfb0c093
	uri /dir/index.html
	op GET
	digest,qop auth
	digest,nc 00000001
	digest,cnonce 0a4f113b
	digest,response 6629fae49393a05397450978507c4ef1
	digest,opaque 5ccc069c403ebaf9f0171e9517f40e41
	digest,algorithm MD5
    }
    AuthParseHtaccess 999 /usr/lib/tclhttpd3.5.0/testdigest
    set DTest [Digest_Request 999 testrealm@host.com /usr/lib/tclhttpd3.5.0/testdigest]
    if {$DTest != 1} {
	error "Failed digest test"
    }
}

# nb: mozilla converts UCS2 to UTF8 for username and password

# auth.tcl
#
# Basic authentication
# This module parses .htaccess files and does the Basic Authentication
# protocol.  There is some generality in here to support multiple
# authentication schemes, but in practice only Basic is used right now.
#
# Each .htaccess file is parsed once and the information is kept in a
# Tcl global array named auth$filename, and upvar aliases this to "info".
# "info" contains the info provided by the .htaccess file ( info(htaccessp,..) )
#
# The AuthUserFile and the AuthGroupFile are parsed and stored
# in authentication array auth$filename.
#
# Authentication arrays need not be associated with physical files.
# To create an authentication array:
#	array set auth$pseudo [list mtime 0 user,$u password group,$g group]
#
# Configuration creates an authentication array authdefault, for bootstrapping.
#
# There is also support for ".tclaccess" files in each directory.
# These contain hook code that define password checking procedures
# that apply at that point in the hierarchy.
#
# Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# Piet Vloet (c) 2001
# Brent Welch (c) 2001
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::auth 2.0
package require base64

set Auth_DigestOnly 0;	# set this to 1 to only use Digest auth

# Crypto packages
if {[catch {package require crypt}] || [catch {crypt foo ba}]} {
    # if we don't have a C library, fall back to pure tcl crypt
    package require tclcrypt
}

# set default user/group files
if {![info exists Config(AuthUserFile)]} {
    set Config(AuthUserFile) default
}
if {![info exists Config(AuthGroupFile)]} {
    set Config(AuthGroupFile) default
}
if {![info exists Config(AuthDefaultFile)]} {
    set Config(AuthDefaultFile) /tmp/tclhttpd.default
}

# Authentication bootstrap.
#
# The /htaccess URL allows admins to create passwords and groups,
# and .htaccess files per directory, however it is necessary to have a
# password to bootstrap this process.
#
# The approach taken is to allow the administrator to give plaintext
# passwords via Config, or (failing that) to generate and Log a random
# password at runtime.  A new random password will be created on each 
# server startup.
#
# Here, we create a default authentication array from Config(Auth)

array set authdefault {mtime -1}
package require httpd::passgen	;# random password generation

if {[info exists Config(Auth)]} {
    foreach {var val} $Config(Auth) {
	if {[string match user,* $var]} {
	    # encrypt the password
	    set salt [Passgen_Salt]
	    set val [crypt $password $salt]
	}
	
	set authdefault($var) $val
    }
} else {
    # we weren't given an Auth Config - generate a default for webmaster
    # and write it to file $Config(AuthDefaultFile)
    
    set webmaster_password [Passgen_Generate]
    set salt [Passgen_Salt]
    set authdefault(user,webmaster) [crypt $webmaster_password $salt]
    set authdefault(group,webmaster) webmaster
    set fd [open $Config(AuthDefaultFile) w 0660]
    puts $fd $webmaster_password
    close $fd
    unset webmaster_password
}

# Auth_InitCrypt --
# Attempt to turn on the crypt feature used to store crypted passwords.

proc Auth_InitCrypt {} {
}

proc Auth_AccessFile {args} {
    Stderr "Auth_AccessFile is obsolete: use Auth_InitCrypt instead"
    Auth_InitCrypt
}

# Auth_Check --
# This looks for access files along the path.
# It returns a "cookie" that is checked by Auth_Verify.
# NOTE: this looks for the lowest (i.e., deepest) access file
# and only returns information about one. Consider changing
# Auth_Check/Auth_Verify to check all files.

proc Auth_Check {sock directory pathlist} {
    global auth
    set cookie {}

    # Make sure we do checks in the root
    if {$pathlist==""} {
	 set pathlist ./
    }

    # Look for the .htaccess files that keep Basic Authtication info
    # or .tclaccess files with a general authorization callback
    set path $directory
    foreach component $pathlist {
        if {![file isdirectory $path]} {
            # Don't bother looking if we are in an "artificial"
            # url domain that isn't mapped to files.
            break
        }
	foreach {name type} {.htaccess Basic .tclaccess Tcl} {
	    set file [file join $path $name]
	    if {[file exists $file]} {
		set cookie [list $type $file]
		# Keep looking for cookie files lower in the directory tree
            }
	}
	set path [file join $path $component]
    }

    # Block access to the access control files themselves.
    # We toss in a block against the .tml files as well,
    # although that isn't strictly clean modularity.
    set tail [file tail $path]
    if {$tail == ".tclaccess" ||
          $tail == ".htaccess" ||
          $tail == ".tml"} {
        set cookie [list Deny $path]
        return $cookie
    }

    return $cookie
}

proc Auth_Verify {sock cookie} {
    if {[llength $cookie] == 0} {
	return 1
    }
    set type [lindex $cookie 0]
    set key [lindex $cookie 1]
    if {$type == "Deny"} {
        return 0
    } else {
        return [AuthVerify$type $sock $key]
    }
}

# Auth_VerifyCallback -- 
#
#       Check for a Basic authorization string, and use a callback
#       to verify the password
#
# Arguments:
#       sock            Handle on the client connection
#       realm           Realm for basic authentication.  This appears
#                       in the password prompt from the browser.
#       callback        Tcl command to check the password.  This gets
#                       as arguments the sock, realm, username and password.
#
# Results:
#                       return 1 or 0, 1 for success.

proc Auth_VerifyCallback {sock realm callback} {
    upvar #0 Httpd$sock data

    if {![info exists data(mime,authorization)]} {
	set ok 0
    } else {
	set parts [split $data(mime,authorization)]
	set type [lindex $parts 0]
	set code [lindex $parts 1]
	if {[string compare $type Basic] != 0} {
	    set user {}
	    set pass {}
	} else {
	    set parts [split [base64::decode $code] :]
	    set user [lindex $parts 0]
	    set pass [lindex $parts 1]
	}
	set ok [eval $callback {$sock $realm $user $pass}]
    }
    if {!$ok} {
	Httpd_RequestAuth $sock Basic $realm
	return 0
    } else {
	set data(auth_type) Basic
	set data(remote_user) $user
	set data(session) $realm,$user
	return 1
    }
}

# AuthVerifyTcl --
#
#       "Tcl" verification uses a .tclaccess file that defines the
#       realm and callback to use to check the password.
#
# Arguments:
#       sock    Handle on the client connection
#       file    Tcl source file that contains set commands for
#               realm and callback
#
# Results:
#       1 for success, 0 for access denied.

proc AuthVerifyTcl {sock file} {
    upvar #0 Httpd$sock data


    # The file contains definitions for the "realm" variable
    # and the "callback" script value.

    set realm Realm
    set callback AuthNullCallback
    catch {source $file}

    return [Auth_VerifyCallback $sock $realm $callback]
}

proc AuthNullCallback {sock realm user pass} {
    upvar #0 Httpd$sock data
    global auth
    if {[info exists auth($realm,$user)]} {
	switch -exact -- $auth($realm,$user) \
	    $pass {
		Stderr "Session: $realm,$user"
		return 1
	    } \
	    PasswordRequired {
		set auth($realm,$user) $pass
		Stderr "Session: $realm,$user"
		return 1
	    } \
	    default {
		return 0
	    }
    } else {
	set auth($realm,$user) PasswordRequired
	return 0
    }
}

# AuthUserOp - see if the user is permitted to perform a requested
# operation (PUT, GET, etc)
# The .htaccess file is searched for the user or a group containing
# the user, to ascertain whether the user could possibly perform
# the op before the proffered credentials are checked.

proc AuthUserOp {sock file op user} {
    upvar #0 auth$file info
    upvar #0 Httpd$sock data

    if {[info exists info(htaccessp,require,$op,group)]} {
	if {![AuthGroupCheck $sock $file \
		  $info(htaccessp,require,$op,group) $user]} {
	    return 0   ;# Not in a required group
	}
    }
    if {[info exists info(htaccessp,require,$op,user)]} {
	if {![AuthUserCheck $sock $file \
		  $info(htaccessp,require,$op,user) $user]} { 
	    return 0   ;# Not the required user
	}
    }
    return 1
}

# AuthVerifyBasic - see if the user is granted access.
# First domain based protection is performed. In the case the
# user is not in the domain user based protection is performed.
# The user must be in a group or mentioned as user. The password
# must match the user's entry.  If neither group nor user are
# required for the operation, then the check passes.

proc AuthVerifyBasic {sock file} {
    upvar #0 auth$file info
    upvar #0 Httpd$sock data

    set user ""

    AuthParseHtaccess $sock $file
    set op $data(proto) ;# GET, POST etc.
    set realm $info(htaccessp,name)

    # check whether the operation is controlled
    if {[info exists info(htaccessp,order,$op)]} {
	if {! [AuthVerifyNet $sock $file $op]} {
	    # it is controlled and network address is excluded
	    Httpd_Error $sock 403
	    return 0
	}
    }
    if {![info exists info(htaccessp,require,$op,group)] &&
	    ![info exists info(htaccessp,require,$op,user)]} {
	# No "require group .." or "require user .." in .htaccess file
	return 1
    }

    set ok 0

    # the operation requires a user or group - check authorization
    if {[info exists data(mime,authorization)]} {
	# client has sent auth data
	set parts [split $data(mime,authorization)]
	set type [lindex $parts 0]

	switch -- [string tolower $type] {
	    digest {
		# unpack the digest request
		Digest_Get $sock $parts

		# check that the proferred user can have access
		set user $data(digest,username)
		set ok [AuthUserOp $sock $file $op $user]

		if {$ok} {
		    # now check the Digest credentials
		    set ok [Digest_Request $sock $realm $file]
		}
	    }
	    basic {
		set code [lindex $parts 1]
		set parts [split [base64::decode $code] :]

		# check that the proferred user can have access
		set user [lindex $parts 0]
		set ok [AuthUserOp $sock $file $op $user]

		if {$ok} {
		    # now check the Basic credentials
		    set pass [lindex $parts 1]
		    set crypt [AuthGetPass $sock $file $user]
		    set salt [string range $crypt 0 1]
		    set crypt2 [crypt $pass $salt]
		    if {[string compare $crypt $crypt2] != 0} {
			set ok 0        ;# Not the right password
		    }
		}
	    }
	    default {
		# this is an unknown auth method - it's not ok, we can't handle it
		set ok 0
	    }
	}
    }

    if {! $ok} {
	# client hasn't sent auth or auth doesn't satisfy
	global Auth_DigestOnly
	if {!$Auth_DigestOnly && ($info(htaccessp,type) == "Basic")} {
	    Httpd_RequestAuth $sock Basic $realm
	} else {
	    # make Digest the default
	    Digest_Challenge $sock $realm $user
	}
    } else {
	# client is permitted and credentials check out
	set data(auth_type) $type
	set data(remote_user) $user
	set data(session) $realm,$user
    }

    return $ok
}

proc AuthUserCheck  {sock file users user } {
    return [expr {[lsearch $users $user] >= 0}]
}

# Parse the AuthGroupFile and find the user in it.
# The information is built up in the array:
#	auth$info(htaccessp,groupfile)
#
# To create an authentication array, unconnected to a physical file:
# array set auth$pseudo [list mtime 0 user,$u password group,$g group]
#
# If the .htaccess file didn't specify a groupfile, it defaults to an authentication
# array authdefault, which contains configured default passwords and groups

proc AuthGroupCheck {sock file groups user} {
    upvar #0 auth$file info
    upvar #0 auth$info(htaccessp,groupfile) group

    if {[catch {file mtime $info(htaccessp,groupfile)} mtime]} {
	set mtime -1	;# the file may not exist
    }

    # Only parse the group file if it has changed
    if {![info exists group(mtime)] || ($mtime > $group(mtime))} {
	catch {unset group(mtime)}	;# unset to catch errors
	foreach i [array names group "group*"] {
	    unset group($i)
	}
	if {[catch {open $info(htaccessp,groupfile)} in]} {
	    return 0
	}
	while {[gets $in line] >= 0} {
	    if {[regexp {^([^:]+):[      ]*(.+)} $line x key value]} {
		set group(group,$key) [split $value " ,"]
	    }
	}
	close $in
	set group(mtime) $mtime
    }

    foreach index $groups {
	if {[info exist group(group,$index)]} {
	    if {[lsearch $group(group,$index) $user] >= 0} {
		return 1
	    }
	}
    }
    return 0
}

# Parse the AuthUserFile and return the user's information.
# The information is built up in the array:
#	auth$info(htaccessp,userfile)
# 
# To create an authentication array, unconnected to a physical file:
# array set auth$pseudo [list mtime 0 user,$u password group,$g group]
#
# If the .htaccess file didn't specify a userfile, it defaults to an authentication
# array authdefault, which contains configured default passwords and groups

proc AuthGetPass {sock file user} {
    upvar #0 auth$file info
    upvar #0 auth$info(htaccessp,userfile) passwd

    if {[catch {file mtime $info(htaccessp,userfile)} mtime]} {
	set mtime -1	;# the file may not exist
    }

    if {![info exists passwd(mtime)] || ($mtime > $passwd(mtime))} {
	catch {unset passwd(mtime)}	;# unset to catch errors
	foreach i [array names passwd "user*"] {
	    unset passwd($i)
	}
	if {[catch {open $info(htaccessp,userfile)} in]} {
	    return *
	}
	while {[gets $in line] >= 0} {
	    if {[regexp {^([^:]+):[      ]*([^:]+)} $line x key value]} {
		set passwd(user,$key) $value
	    }
	}
	close $in
	set passwd(mtime) $mtime
    }
    if {[info exists passwd(user,$user)]} {
	return $passwd(user,$user)
    } else {
	return *
    }
}

# Check the allow/deny lists for this operation

proc AuthVerifyNet {sock file op} {
    upvar #0 auth$file info
    set order [split $info(htaccessp,order,$op) ,]
    set peer [fconfigure $sock -peername]
    set rname [string tolower [lindex $peer 1]]
    set raddr [lindex $peer 0]
    set ok 0
    foreach way $order {
	if {![info exists info(htaccessp,network,$way,$op)]} {
	    continue
	}
	foreach addr $info(htaccessp,network,$way,$op) {
	    if {[AuthNetMatch $sock $addr $rname $raddr]} {
		if {[string compare $way "allow"] == 0} {
		    set ok 1
		} else {
		    set ok 0
		}
	    }
	}
    }
    if {! $ok} {
	Log $sock AuthVerifyNet "access denied to $rname in [file tail [file dirname $file]]"
    }
    return $ok
}

proc AuthNetMatch {sock addr rname raddr} {
    if {[string compare $addr "all"] == 0} {
	return 1
    }
    if {[string match *$addr $rname] || [string match ${addr}* $raddr]} {
	return 1
    }
    return 0
}

# Parse the htaccess file.  Uhler would probably regsub/subst this,
# but here we just call a Tcl proc to handle each "command" in the file.
# The information is built up in the info array.

proc AuthParseHtaccess {sock file} {
    upvar #0 auth$file info
    set mtime [file mtime $file]
    if {![info exists info] || ($mtime > $info(htaccessp,mtime))} {
	# Parse .htaccess file
	foreach i [array names info "htaccessp*"] {
	    unset info($i)
	}
	set info(htaccessp,mtime) $mtime

	global Config
	set info(htaccessp,userfile) $Config(AuthUserFile)
	set info(htaccessp,groupfile) $Config(AuthGroupFile)

	set info(htaccessp,name) Digest
	if {[catch {open $file} in]} {
	    return 1
	}
	set state [list vars]
	foreach line [split [read $in] \n] {
	    if {[regexp ^# $line] || [string length [string trim $line]] == 0} {
		continue
	    }
	    if {[regexp <(.+)> $line x tag]} {
		set line $tag
	    }
	    set words [split $line]
	    set cmd [string tolower [lindex $words 0]]
	    if {[catch {
		eval {Ht-$cmd auth$file} [lrange $words 1 end]
	    } err]} {
		Log $sock Error $err
	    }
	}
	close $in
    }
    return 1
}
proc Ht-authtype {infoName type} {
    upvar #0 $infoName info
    set info(htaccessp,type) $type
}
proc Ht-authname {infoName name} {
    upvar #0 $infoName info
    set info(htaccessp,name) $name
}

proc Ht-authuserfile {infoName file} {
    upvar #0 $infoName info
    set info(htaccessp,userfile) $file
}

proc Ht-authgroupfile {infoName file} {
    upvar #0 $infoName info
    set info(htaccessp,groupfile) $file
}

proc Ht-limit {infoName args} {
    upvar #0 $infoName info
    set info(htaccessp,limit) $args       ;# List of operations, GET, POST, ...
}

proc Ht-/limit {infoName args} {
    upvar #0 $infoName info
    set info(htaccessp,limit) {}
}

proc Ht-require {infoName key list} {
    upvar #0 $infoName info
    if {![info exists info(htaccessp,limit)]} {
	set info(htaccessp,limit) {}
    }
    foreach op $info(htaccessp,limit) {
	if {![info exists info(htaccessp,require,$op,$key)]} {
		set info(htaccessp,require,$op,$key) {}
	    }
	    foreach a $list {
		lappend info(htaccessp,require,$op,$key) $a
	    }
     }
}

proc Ht-order {infoName value} {
    upvar #0 $infoName info
    if {![info exists info(htaccessp,limit)]} {
	set info(htaccessp,limit) {}
    }
    foreach op $info(htaccessp,limit) {
	if {[info exists info(htaccessp,order,$op)]} {
		 unset info(htaccessp,order,$op)
	}
	set info(htaccessp,order,$op) $value
    }
}

proc Ht-deny {infoName args} {
    HtByNet $infoName deny $args
}
proc Ht-allow {infoName args} {
    HtByNet $infoName allow $args
}
proc HtByNet {infoName how list} {
    upvar #0 $infoName info
    if {![info exists info(htaccessp,limit)]} {
	set info(htaccessp,limit) {}
    }
    if {[string compare [lindex $list 0] "from"] == 0} {
	set list [lrange $list 1 end]
    }
    foreach op $info(htaccessp,limit) {
	if {![info exists info(htaccessp,network,$how,$op)]} {
	    set info(htaccessp,network,$how,$op) {}
	}
	foreach a $list {
	    lappend info(htaccessp,network,$how,$op) [string tolower $a]
	}
    }
}

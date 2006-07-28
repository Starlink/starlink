# passcheck.tcl
# 
package provide httpd::passcheck 1.0

namespace eval passcheck { }

# Copyright(c) Genesys Software Romania srl., 2000
#
# License is granted here to use this code for any purpose, 
# No warranties included.
#
# 
# 
# Provides password checking against various systems.
# Useful with TCLHTTPD server, available from http://www.tcl.tk/
#
# The purpose is to avoid maintaining duplicate password tables
# for tclhttpd, and re-use instead existing systems available in your local net.
# This way users dont need to remember many passwords - just use known passwords.
#
#
# Currently implemented are pop3 and odbc checkers. 
#
# Use pop3 pass checker to authenticate users against an pop mail server
# it requires pop3 package 
# 
# Use odbc pass checker to perform authentication against a ODBC datasource 
# available locally to the web server. 
# 
# 
# You can add your passcheckers by providing procedures with the following
# syntax and name conventioni: (we consider thi will authenticate against LDAP:
#
# proc ::passcheck::verify_ldap {sock realm user pass {config {}}} { .... return 1 or 0 }
# 
# the sock, realm, user, pass arguments are the same as received by a .tclaccess "callback"
# command.
#
# the config argument should provide significant configuration options for the checker.
# e.g. for pop3 it should be a mail server machine name, running pop3 service, 
# and for odbc - a datasource name.
#
# Other usefull commands are here - to see wich types of checkers are available, 
# to clear password caches
# for specific or all realms, to see wich realms and how are configured. 
# Author: Cezar Totth <cezar@genesys.ro>
#
set ::passcheck::comment {

 #
 # You'll probably need to add these in your tclhttpd.rc file..
 #
 package require httpd::passcheck
 #
 ################# POP3 example 
 # Configure checker to use pop3 authentication for the realm named "LocalMail" against 
 # an existing local pop server. 

 set pop3server 192.168.50.5 
 ::passcheck::setChecker LocalMail pop3 $pop3server

 #
 ######################## 
 #Then  you can specify within any .tclaccess file:
 realm LocalMail
 callback ::passcheck::Verify

 ########################
 
 ################# ODBC example ##########################
 # Configure it to authenticate against a datasource.
 # The realm will be named "DbServer"
 ::passcheck::setChecker DbServer odbc $datasource

 ###### and in .tclaccess files: 
 realm DbServer
 callback ::passcheck::Verify

}

################################  End of comment here ...

proc ::passcheck::setChecker {realm passcheckerName {confopt {}}} {
  set ::passcheck::_Config($realm) [list verify_$passcheckerName $confopt]
}

proc ::passcheck::unsetConf {realm} {
  catch {unset ::passcheck::_Config($realm)}
}
#
# The pass checker configuration for all realms. 
#
set ::passcheck::DefaultChecker [list verify_deny {}]

proc ::passcheck::getChecker realm { 
  upvar #0 ::passcheck::_Config($realm) myConf
  if {[info exists myConf]} { return $myConf }
  return ${::passcheck::DefaultChecker}
}

proc ::passcheck::getCheckerConf {realm} { return  [lindex [::passcheck::getChecker $realm] 1] }
proc ::passcheck::getCheckerName {realm} { return  [lindex [::passcheck::getChecker $realm] 0] }

#
# Returns the available types of passcheckers (e.g. pop3, odbc, never, allways..)
proc ::passcheck::getCheckerTypes {} {
  set toR {}
  foreach cname [info commands ::passcheck::verify_*] {
    lappend toR [lindex [split $cname _] 1]
  }
  return $toR
}

#
# returns wich realms are controlled by our pass checkers
proc ::passcheck::getRealms {} {
  return [array names ::passcheck::_Config] 
}

#
# Use this to clear password caches. Needed after users changed their passwords.
# No realm specified means to clear all caches of all realms.
proc ::passcheck::clearCache {{realm {}}} {
  if {![llength $realm]} { 
     set rlist [info vars ::passcheck::_passCache_*]
  } else { set rlist ::passcheck::_passCache_$realm } 
  foreach carr $rlist {
     catch { unset $carr } 
  }
}

#
# this is to be specified from .tclaccess files on tclhttpd.
# add lines:
# 
# set realm ARealmName
# set callback ::passcheck::Verify
# 
proc ::passcheck::Verify {socket realm user pass}  {
   upvar #0 ::passcheck::_passCache_$realm pcache
   set kpass [crypt $pass 91] 
   if {[info exists pcache($user)]} {
     if { "$kpass" == "$pcache($user)" } { 
       set ::env(REMOTE_USER) $user
       return 1 
     } else { unset pcache($user) }
   } 
   set pcheck [::passcheck::getChecker $realm]
   set tocall [list [lindex $pcheck 0] $socket $realm $user $pass [lindex $pcheck 1]]
   # puts "will call: $tocall"
   set rez 0 
   catch { set rez [eval $tocall] } 
   if {$rez} {
       set pcache($user) $kpass
       set ::env(REMOTE_USER) $user 
   }
   return $rez
}

############################## Basic Auth --> Pop3 authentication ######################
#
# By default the tcl server is considered to run on the email server.
# On most Unix-like systems this means authentication is made against
# the local system
proc ::passcheck::verify_pop3 {socket realm user pass {conf localhost}} {
   if {[catch {::pop3::close [::pop3::open $conf $user $pass]}] } { 
       return 0 
   } 
   return 1
}

############################# TCLODBC Datasource authentication #######################
proc ::passcheck::verify_odbc {socket realm user pass {conf passcheck}} {
  if {[catch {
         database _passCheck_db$conf$user $conf $user $pass
      } ] } { return 0 }
  _passCheck_db$conf$user disconnect
  return 1
}
#
############################ Dummy pass checkers ######################################
# These are dummy pass checkers. Usefull in automated tests, or to temporarly lock/unlock
# realms
proc ::passcheck::verify_deny  {socket realm user pass {conf {}}} { return 0 }
proc ::passcheck::verify_allow {socket realm user pass {conf {}}} { return 1 }

########################### external packages availability #######################
# 
# warning messages. Shall it go to server's log too? 
proc ::passcheck::warning message { 
    puts "passcheck: $message" 
}
#
#
# Is better to have crypt handy  to cache passwords with more confidence. 
if { [catch { package require crypt } ] } {
  # we provide a dummy crypt...
  package require tclcrypt
  #::passcheck::warning {no crypt package available! passwords cached in cleartext!}
  #proc crypt {word salt} { return "${word}$salt" }
}

#
# checking for POP3 
if { [catch { package require pop3 } ] } { 
  ::passcheck::warning {no POP3 pass check available! verify pop3 package}
  # rename pop3_verify pop3_notAvail 
}

#
# checking for TCLODBC 
if { [catch { package require tclodbc } ] } {
  ::passcheck::warning {no ODBC pass check available! verify tclodbc package}
  # rename odbc_verify odbc_notAvail 
}

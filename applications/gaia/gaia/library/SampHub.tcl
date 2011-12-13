#+
#  Name:
#     SampHub

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Provides methods for communicating with a SAMP hub.

#  Description:
#     This class serves as a proxy for a remote SAMP hub process.
#     On construction it attempts to locate a SAMP hub's Standard
#     Profile XML-RPC server by searching for and reading the ~/.samp
#     file.  When constructed, hub methods can be executed using the
#     execute method.  For instance, the following registers and finds
#     the IDs of registered clients:
#
#        samp::SampHub hub
#        hub register reginfo
#        set privateKey $reginfo(samp.private-key)
#        set clients [hub execute getRegisteredClients $privateKey]
#
#     See http://www.ivoa.net/samp/ for a discussion of the SAMP protocol.

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  History:
#     18-JUN-2009 (MBT)
#        Original version, adapted from PlasticHub.tcl.
#     {enter_further_changes_here}

#-

#  Import all the packages we need for XMLRPC.
package require XMLRPC

itcl::class samp::SampHub {

   #  Constructor:
   #  ------------
   constructor {args} {
      eval configure $args

      #  Read the SAMP registration information from the .samp file.
      read_props_ [rendezvous_file] samp_properties
      set hub_url_ $samp_properties(samp.hub.xmlrpc.url)
      set samp_secret_ $samp_properties(samp.secret)

      #  Create functions in this namespace matching the services that
      #  the hub should provide.
      set hubcmds {
         { register {secret string} }
         { unregister {private_key string} }
         { declareMetadata {private_key string metadata struct} }
         { getMetadata {private_key string client_id string} }
         { declareSubscriptions {private_key string subscriptions struct} }
         { getSubscriptions {private_key string client_id string} }
         { getRegisteredClients {private_key string} }
         { getSubscribedClients {private_key string mtype string} }
         { notify {private_key string recipient_id string message struct} }
         { notifyAll {private_key string message struct} }
         { call {private_key string recipient_id string msg_tag string
                 message struct} }
         { callAll {private_key string msg_tag string message struct} }
         { callAndWait {private_key string recipient_id string message struct
                        timeout string} }
         { reply {private_key string msg_id string response struct} }
         { setXmlrpcCallback {private_key string url string} }
         { ping {} }
      }
      foreach hubcmd $hubcmds {
         set name [lindex $hubcmd 0]
         set params [lindex $hubcmd 1]
         samp_xmlrpc_create_ samp.hub.$name -proxy $hub_url_ -params $params
      }
   }

   #  Methods:
   #  --------

   #  Executes one of the hub methods using XML-RPC on the remote hub.
   #  See the SAMP specification for the descriptions of these methods.
   public method execute {hubMethod args} {
      return [eval samp.hub.$hubMethod $args]
   }

   #  Registers with the hub and populates the given reginfo array with
   #  the associated registration information.
   public method register {reginfo} {
      upvar $reginfo _reginfo
      array set _reginfo [execute register $samp_secret_]
   }

   #  Common Procedures:
   #  ------------------

   #  Returns 1 if a SAMP hub is apparently running, 0 if not.
   public proc is_hub_running {} {
      return [file exists [rendezvous_file]]
   }

   #  Returns the location of the .samp file which contains information
   #  concerning the currently-running hub.
   public proc rendezvous_file {} {
      if { [string match {CYGWIN*} $::tcl_platform(os)] } {
         set dir $::env(USERPROFILE)
      } {
         set dir $::env(HOME)
      }
      return [file join $dir .samp]
   }

   #  Reads the properties from a file with the format of the SAMP
   #  Standard Profile lockfile.
   protected proc read_props_ {filename props} {
      upvar $props _props
      set fid [open $filename r]
      while {[gets $fid line] >= 0} {
         if {[regexp {^([a-zA-Z0-9_\.\-]*)=(.*)$} [string trim $line] \
                                                  all key value]} {
            set _props($key) $value
         }
      }
      close $fid
   }

   #  Create a Tcl wrapper for an XMLRPC method call.  This is a thin
   #  wrapper around SOAP::create, and has the same usage, but it
   #  ensures that only SAMP-friendly types are passed to the hub
   #  (i.e. it avoids elements like <int> and <double> which the XMLRPC
   #  system would otherwise write for elements that looked like those
   #  types).
   proc samp_xmlrpc_create_ {args} {
      set args [linsert $args 1 \
              -wrapProc [namespace origin samp_xmlrpc_request_] \
              -parseProc [namespace origin ::SOAP::parse_xmlrpc_response]]
      return [uplevel 1 "SOAP::create $args"]
   }

   #  Postprocesses a chunk of XML, replacing non-SAMP-friendly tags
   #  (<int>, <double> etc) with SAMP-friendly ones (<string>).
   public proc samp_xmlrpc_request_ {procVarName args} {
      upvar $procVarName procvar
      set soap_invoke [list SOAP::xmlrpc_request $procVarName]
      foreach arg $args {
         lappend soap_invoke $arg
      }
      set result [eval $soap_invoke]
      set tag_re {(</?)int|i4|boolean|double|float|dateTime\.iso8601|base64(>)}
      return [regsub -all $tag_re $result {\1string\2}]
   }

   #  Private variables: (available to instance)
   #  ------------------
   private variable hub_url_
   private variable samp_secret_
}

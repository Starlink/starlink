#+
#  Name:
#     SampClient

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Controls an XML-RPC server for use with the SAMP protocol.

#  Description:
#     Instances of this class act as SAMP clients.
#     The class provides common methods which start up and shut down
#     the HTTPD server which provides the necessary communications
#     for SAMP callbacks.  This server is started up automatically
#     by the first instance that is registered with the hub.

#  Invocations:
#
#     SampClient object_name [configuration options]
#
#  This creates an instance of a SampClient object.  The return is
#  the name of the object.
#
#     object_name method arguments
#
#  Performs the given method on this object.

#  Configuration options:
#     -agents:
#        A list of zero or more agents which respond to incoming messages.
#        These operate in addition to the client_tracker agent.
#        Each object listed here should have a method
#        get_subscribed_mtypes which returns the supported MTypes, and
#        for each one a method named after the MType with arguments:
#           sender_id (string)
#           param_list (list of the form {key value key value ...})
#        with a return value giving the samp.result value for a
#        successful message Response in the form {key value key value}.
#        Such methods may (and should) throw Tcl errors in the case of
#        failure.  An example agent implementation might look like this:
#
#           itcl::class EchoSampAgent {
#              public method get_subscribed_mtypes {} {
#                 return {test.echo}
#              }
#              public method test.echo {sender_id param_list} {
#                 array set params $param_list
#                 return [list text $params(text)]
#              }
#           }
#
#     -client_tracker:
#        An agent which keeps track of the clients currently
#        registered with the hub.  This should usually be a
#        samp::ClientTracker object.
#     -metadata:
#        SAMP metadata map in the form {key value key value ...}

#  Methods:
#     See individual method declarations.

#  Inheritance:
#     This object inherits no other classes.

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
#     PWD: Peter Draper
#     {enter_new_authors_here}

#  History:
#     18-JUN-2009 (MBT):
#        Original version, adapted from PlasticApp.tcl.
#     {enter_further_changes_here}

#-

#  Import all the packages we need to support tclsoap.
package require XMLRPC::Domain
package require rpcvar
package require httpd
package require httpd::url
package require httpd::threadmgr
package require httpd::utils
package require httpd::version
package require httpd::counter
package require httpd::doc
package require httpd::log

#  Server files should be private to user.
global Config
set Config(AuthDefaultFile) [utilGetConfigFilename .skycat tclhttpd.default]
package require httpd::auth
package require httpd::mtype

namespace import -force rpcvar::*

itcl::class samp::SampClient {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      set client_tracker [code [samp::ClientTracker #auto]]
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor {
      unregister
   }

   #  Methods:
   #  --------

   #  Attempts to register with a running hub.
   #  Generates an error if this is not possible (most likely because
   #  the hub is not running).
   public method register {} {

      #  Get an object representing a running hub.
      set hub_ [samp::SampHub #auto]

      #  Get the base URL of the XML-RPC server.
      set server_url [get_server_url]

      #  Add a trailing string as a namespacing measure to ensure that
      #  this object does not receive messages intended for other
      #  clients.
      set subname samp[incr instance_count]
      set callable_url $server_url/$subname

      #  Prepare procedures which will handle incoming XML-RPC calls
      #  to this callable client, and ensure that such requests
      #  get delivered to it.
      #  The procedures are in a namespace specific to this instance.
      set namespace ::samp::clients::$subname
      namespace eval ${namespace} {
         XMLRPC::export samp.client.receiveNotification
         XMLRPC::export samp.client.receiveCall
         XMLRPC::export samp.client.receiveResponse
      }
      proc ${namespace}::samp.client.receiveNotification \
            {private_key sender_id message} "
         $this client_receive_message \$sender_id \$message
      "
      proc ${namespace}::samp.client.receiveCall \
            {private_key sender_id msg_id message} "
         set response \[$this client_receive_message \$sender_id \$message\]
         $this client_reply \$msg_id \$response
      "
      proc ${namespace}::samp.client.receiveResponse \
            {private_key responder_id msg_tag response} "
         $this client_receive_response \$responder_id \$msg_tag \$response
      "
      XMLRPC::Domain::register -prefix /$subname -namespace $namespace

      #  Register for communication with the remote hub.
      foreach agent [get_all_agents_] {
         foreach mtype [{*}$agent get_subscribed_mtypes] {
            set subscriptions($mtype) [rpcvar struct {}]
         }
      }
      $hub_ register reginfo
      set_reginfo_ reginfo
      configure -client_tracker $client_tracker

      #  Inform hub of metadata and subscriptions.
      $hub_ execute setXmlrpcCallback $private_key_ $callable_url
      update_metadata_
      update_subscriptions_

      #  Log existence of server.
      puts "SAMP service at $callable_url"
   }

   #  Unregisters this instance with the SAMP hub, if it is currently
   #  registered.  If it is not currently registered, nothing happens.
   public method unregister {} {
      if {[is_registered]} {
         if {[catch {
            $hub_ execute unregister $private_key_
         } msg]} {
            puts "Trouble unregistering from SAMP: $msg"
         }
         set_reginfo_ {}
      }
   }

   #  This method is called when the hub sends a message (call or notify)
   #  which should be handled by this instance of the class.  Requests
   #  are handled by handing them off to an appropriate method of one of
   #  the agents.  The return value is a SAMP response map (as a list),
   #  which may indicate success or failure.
   public method client_receive_message {sender_id message} {

      #  Handle the message and come up with a success or error result.
      set status [catch {

         #  Decompose the message.
         array set msg $message
         set mtype $msg(samp.mtype)
         set params $msg(samp.params)

         #  Check each agent to see if it represents a subscription to
         #  the MType in question and if so execute that with
         #  appropriate arguments.
         foreach agent [get_all_agents_] {
            if {[lsearch [{*}$agent get_subscribed_mtypes] $mtype] >= 0} {
               return [eval [list {*}$agent $mtype $sender_id $params]]
            }
         }

         #  No luck.  This ought not to happen, since the hub should only
         #  be sending messages with MTypes we have previously identified as
         #  names of methods in one of the agents.
         error "No handler for MType $mtype"
      } result]

      #  Translate the result into a SAMP response object.
      if {$status == 0 || $status == 2} {
         set response(samp.status) samp.ok
         set response(samp.result) $result
      } {
         set response(samp.status) samp.error
         set response(samp.error) [rpcvar struct [list \
            samp.code $status \
            samp.errortxt [regsub {\n.*} $result {}] \
            samp.debugtxt $::errorInfo \
            samp.code $::errorCode \
         ]]
      }
      return [array get response]
   }

   #  Invoked from samp.client.receiveCall to pass the reply back
   #  asynchronously to the hub.
   public method client_reply {msg_id response} {
      $hub_ execute reply $private_key_ $msg_id $response
   }

   #  Invoked from samp.client.receiveResponse to perform actions in
   #  response to a call which was dispatched asynchronously.
   public method client_receive_response {responder_id msg_tag response_list} {
      array set response $response_list
      set responder_name [{*}$client_tracker get_name $responder_id]
      set status $response(samp.status)

      #  Log response status if required.
      if {$status == "samp.ok"} {
         #  successful receipt
      } else {
         set status $response(samp.status)
         set errortxt ""
         set debugtxt ""
         if {[info exists response(samp.error)]} {
            array set errinfo $response(samp.error)
            if {[info exists errinfo(samp.errortxt)]} {
               set errortxt $errinfo(samp.errortxt)
            }
            if {[info exists errinfo(samp.debugtxt)]} {
               set debugtxt $errinfo(samp.debugtxt)
            }
         }
         puts "$status response from $responder_name: $errortxt"
         if {$debugtxt != ""} {
            puts $debugtxt
         }
      }

      #  If there is a pending callback, handle it.  The callback code
      #  ([lindex $done_commands_($msg_tag) 0]) is executed only when
      #  responses from all the expected recipients have been received.
      set command $done_commands_($msg_tag)
      set responder_ids [lindex $command 0]
      set code [lindex $command 1]
      set rindex [lsearch -exact $responder_ids $responder_id]
      if {$rindex >= 0} {
         set responder_ids [lreplace $responder_ids $rindex $rindex]
      } else {
         puts "Response for unsent message??"
      }
      if {[llength $responder_ids] == 0} {
         eval $code
         array unset done_commmands $msg_tag
      } else {
         set done_commands_($msg_tag) [list $responder_ids $code]
      }
   }

   #  Sends a notification (message with no response reqired) to one
   #  or all other clients.  If recipient_id is empty, the message
   #  is sent to all other clients.
   public method notify {mtype param_list recipient_id} {
      set msg [message_ $mtype $param_list]
      if {[string length $recipient_id] > 0} {
         $hub_ execute notify $private_key_ $recipient_id $msg
      } {
         $hub_ execute notifyAll $private_key_ $msg
      }
   }

   #  Sends an asynchronous call (message with response required) to one
   #  or all other clients.  If recipient_id is empty, the message is
   #  sent to all other clients.  The code in done_command, if any,
   #  is executed when all the responses have been received.
   public method call {mtype param_list recipient_id {done_command {}}} {
      set msg [message_ $mtype $param_list]
      set tag [create_tag_]
      if {$recipient_id != ""} {
         $hub_ execute call $private_key_ $recipient_id $tag $msg
         if {$done_command != ""} {
            set done_commands_($tag) [list $recipient_id $done_command]
         }
      } {
         set call_list [$hub_ execute callAll $private_key_ $tag $msg]
         array set calls $call_list
         set done_commands_($tag) [list [array names calls] $done_command]
      }
   }

   #  Makes a synchronous call to another client.  If the optional timeout
   #  argument is given it indicates a maximum wait time for the response.
   public method callAndWait {mtype param_list recipient_id {timeout {0}}} {
      set msg [message_ $mtype $param_list]
      return [$hub_ execute callAndWait $private_key_ \
                    $recipient_id $msg $timeout]
   }

   #  Standard method to report MTypes served by this object it its
   #  capacity as an agent.
   public method get_subscribed_mtypes {} {
      return {
         samp.hub.event.shutdown
         samp.app.ping
      }
   }

   #  Implementation for samp.hub.event.shutdown MType.
   public method samp.hub.event.shutdown {sender_id params} {
      puts "SAMP hub stopped"
      set_reginfo_ {}
   }

   #  Implementation for samp.app.ping MType.
   public method samp.app.ping {sender_id params} {
   }

   #  Returns a boolean value indicating whether the client is
   #  currently [believed to be] registered with a live hub.
   public method is_registered {} {
      return [expr {$private_key_ != ""}]
   }

   #  Adds a callback command to be executed when the SAMP registration
   #  status changes.  An attempt will be made to execute the given command
   #  when this application registers or unregisters with a hub
   #  (or when that may have happened).
   public method reg_change_command {cmd} {
      lappend reg_change_commands_ $cmd
   }

   #  Performs the requested callbacks when the registration status
   #  may have changed.
   protected method inform_samp_reg_ {} {
      foreach cmd $reg_change_commands_ {
         catch {eval $cmd}
      }
   }

   #  Internal method to store registration information.
   protected method set_reginfo_ {reginfo} {
      if {[string length $reginfo] == 0} {
         set private_key_ {}
         set self_id_ {}
      } {
         upvar $reginfo rinfo
         set private_key_ $rinfo(samp.private-key)
         set self_id_ $rinfo(samp.self-id)
      }
      inform_samp_reg_
   }

   #  Returns a list of all the agents that will be used to respond to
   #  hub messages.
   private method get_all_agents_ {} {
      lappend ags $this
      if {[string length $client_tracker] > 0} {
         lappend ags $client_tracker
      }
      foreach ag $agents {
         lappend ags $ag
      }
      return $ags
   }

   #  Informs the hub of this client's current subscriptions if registered.
   private method update_subscriptions_ {} {
      foreach agent [get_all_agents_] {
         foreach mtype [{*}$agent get_subscribed_mtypes] {
            set subs($mtype) [rpcvar struct {}]
         }
      }
      if {[is_registered]} {
         $hub_ execute declareSubscriptions $private_key_ [array get subs]
      }
   }

   #  Informs the hub of this client's current metadata if registered.
   private method update_metadata_ {} {
      if {[is_registered]} {
         $hub_ execute declareMetadata $private_key_ $metadata
      }
   }

   #  Creates a message given the MType and parameters.
   private method message_ {mtype params} {
      set msg(samp.mtype) $mtype
      set msg(samp.params) [rpcvar struct $params]
      return [array get msg]
   }

   #  Creates a unique message tag for SAMP calls.
   private method create_tag_ {} {
      return "$this-[incr tag_count_]"
   }

   #  Common Procedures:
   #  ------------------

   #  Returns the base URL for the HTTPD server servicing XML-RPC requests.
   #  If no server is currently running, this will attempt to start one up.
   public proc get_server_url {} {
      if {! [info exists server_url_]} {
          start_server
      }
      return $server_url_
   }

   #  Starts the HTTPD server.  Will fail if this object's server is
   #  already running.
   public proc start_server {} {
      if {[info exists server_url_]} {
          error "server is already running at $server_url_"
      }

      #  Start a server.  Scan ports within a range starting at default_port
      #  until we find one that we can use.
      set hostname [get_hostname_]
      Httpd_Init
      set done 0
      set errmsg ""
      for { set port $default_port } { $port - $default_port < 16 } \
          { incr port } {
         if { [expr ! [catch {Httpd_Server $port $hostname} errmsg]] } {
            set done 1
            break
         }
      }
      if { ! $done } {
         error "No free port in range $default_port .. $port ($errmsg)"
      }
      Counter_Init 60

      #  Store the server URL; this also serves as a flag that the server
      #  is running.
      set server_url_ "http://${hostname}:${port}"

      #  Trap signals to perform a tidy shutdown if this hasn't been done
      #  already.  This is necessary otherwise we might fail to inform
      #  the hub that we have stopped listening.
      if {! $trapping_} {
         set trapping_ 1
         set signals [list SIGHUP SIGINT SIGQUIT SIGTERM]
         signal trap $signals "
            catch {
               samp::SampClient::stop_server
               puts stderr \"GAIA aborts, signal: %S\"
            }
            exit
         "
      }

      #  Log message.
      puts "Started XML-RPC server for SAMP at $server_url_"
   }

   #  Informs the SAMP hub that any instances of this class which
   #  are registered as clients will no longer be listening, and then
   #  stops the HTTPD server.  If the server is already stopped and
   #  no instances are registered with the hub, does nothing.
   #  Catches its own errors, so is guaranteed(?) to execute cleanly.
   public proc stop_server {} {
      if {[catch {
         foreach app [itcl::find objects -class samp::SampClient] {
            $app unregister
         }
         if {[info exists server_url_]} {
            Httpd_Shutdown
            puts "XML-RPC server stopped at $server_url_"
            unset server_url_
         }
      } msg]} {
         puts "Trouble stopping server: $msg"
      }
   }

   #  Returns the hostname of the local machine.
   private proc get_hostname_ {} {
      if {[catch {set name [info hostname]}]} {
         return localhost
      } else {
         return $name
      }
   }

   #  Public variables:
   #  ----------------
   public variable client_tracker {} {
      {*}$client_tracker configure -self_id $self_id_ \
                                -private_key $private_key_
      {*}$client_tracker configure -hub [code $hub_]
   }
   public variable metadata {samp.name gaia_app} {
      update_metadata_
   }
   public variable agents {} {
      update_subscriptions_
   }

   #  Private variables: (available to instance)
   #  -----------------
   private variable hub_
   private variable private_key_ {}
   private variable self_id_ {}
   private variable reg_change_commands_ {}
   private variable tag_count_ 0
   private variable done_commands_

   #  Common variables:
   #  -----------------
   private common server_url_
   private common trapping_ 0
   private common instance_count 0
   public common default_port 8029
}

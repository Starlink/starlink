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
#     SampClient object_name agents [configuration options]
#
#  This creates an instance of a SampClient object.  The return is
#  the name of the object.
#
#  The agents constructor argument provides the implementation
#  of the supported MTypes.  This should contain a list of zero or
#  more objects with methods whose names are the actual MTypes
#  for messages which are or may be subscribed to.
#  Such methods have two arguments:
#     senderId (string)
#     params (list of the form {key value key value ...})
#  and the return value is the message Response in the form
#  {key value key value ...}.  It should also provide a method
#  get_subscribed_mtypes which lists the MType methods.
#  For instance an agent which implements
#  only an echo message might look like this:
#
#     itcl::class BasicSampAgent {
#        public method test.echo {sender_id params} {
#           array set param_array $params
#           return [list text $param_array(text)]
#        }
#        public method get_subscribed_mtypes {} {
#           return {test.echo}
#        }
#     }
#
#     object_name method arguments
#
#  Performs the given method on this object.

#  Configuration options:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     MBT: Mark Taylor
#     PWD: Peter Draper
#     {enter_new_authors_here}

#  History:
#     18-JUN-2009 (MBT)
#        Original version, adapted from PlasticApp.tcl.
#     {enter_further_changes_here}

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
# set Config(AuthDefaultFile) [utilGetConfigFilename .skycat tclhttpd.default]
package require httpd::auth
package require httpd::mtype

namespace import -force rpcvar::*

itcl::class samp::SampClient {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {agents args} {
      set user_agents_ $agents
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
      set subname plastic[incr instance_count]
      set callable_url $server_url/$subname

      #  Prepare procedures which will handle incoming XML-RPC calls
      #  to this callable client, and ensure that such requests
      #  get delivered to it.
      #  The procedures are in a namespace specific to this instance.
      set namespace ::samp::clients::$subname
      namespace eval ${namespace} {
         XMLRPC::export receiveNotification
         XMLRPC::export receiveCall
         XMLRPC::export receiveResponse
      }
      proc ${namespace}::receiveNotification {sender_id message} "
         $this client_receiveMessage \$sender_id \$message
      "
      proc ${namespace}::receiveCall {sender_id msg_id message} "
         set response \[$this client_receiveMessage \$sender_id \$message\]
         $hub_ execute reply $private_key_ \$msg_id \$response
      "
      proc ${namespace}::receiveResponse {responder_id msg_tag response} {
         error "Asynchronous calls not dispatched by this application"
      }
      XMLRPC::Domain::register -prefix /$subname -namespace $namespace

      #  Register for communication with the remote hub.
      foreach agent [get_all_agents_] {
         foreach mtype [$agent get_subscribed_mtypes] {
            set subscriptions($mtype) [rpcvar struct {}]
         }
      }
      $hub_ register reginfo
      set_reginfo_ reginfo
      configure -client_tracker $client_tracker

      #  Inform hub of metadata and subscriptions.
      $hub_ execute declareMetadata $private_key_ $metadata
      $hub_ execute setXmlrpcCallback $private_key_ $callable_url
      if {[array exists subscriptions]} {
         $hub_ execute declareSubscriptions $private_key_ \
                                            [array get subscriptions]
      }

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
   #  the agents.  The return value is a SAMP response map, which may
   #  indicate success or failure.
   public method client_receiveMessage {sender_id message} {

      #  Handle the message and come up with a success or error result.
      set status catch {

         #  Decompose the message.
         set mtype $message(samp.mtype)
         set params $message(samp.params)

         #  Check each agent to see if it represents a subscription to
         #  the MType in question and if so execute that with
         #  appropriate arguments.
         foreach agent [get_all_agents] {
            if {[lsearch [$agent get_subscribed_mtypes] $mtype] >= 0} {
               return [success_response [eval $agent $sender_id $params]]
            }
         }

         #  No luck.  This ought not to happen, since the hub should only
         #  be sending messages with MTypes we have previously identified as
         #  names of methods in one of the agents.
         error "No handler for MType $mtype"
      } result info

      #  Translate the result into a SAMP response object.
      if {$status == 0} {
         set response(samp.status) samp.ok
         set response(samp.result) $result
      } {
         set response(samp.status) samp.error
         set response(samp.error) [rpcvar struct [list \
            samp.code $status \
            samp.errortxt $result \
            samp.debugtext $info(-errorinfo) \
            tcl.line $info(-errorline) \
         ]
      }
   }

   #  Sends a notification (message with no response reqired) to one
   #  or all other clients.  If recipient_id is null, the message
   #  is sent to all other clients.
   public method notify {mtype paramsVar {recipient_id {}}} {
      upvar $paramsVar params
      set msg [message $mtype [array get params]]
      if {[string length $recipient_id] > 0} {
         $hub_ execute notify $private_key_ $recipient_id $msg
      } {
         $hub_ execute notifyAll $private_key_ $msg
      }
   }

   #  Sends a call (message with response required) to another client.
   #  If the optional timeout argument is given it indicates a maximum
   #  wait time for the response.
   public method call {mtype paramsVar recipient_id {timeout {0}}} {
      upvar $paramsVar params
      set msg [message $mtype [rpcvar struct [array get params]]]
      return [$hub_ execute callAndWait $private_key_ \
                    $recipient_id $msg $timeout]
   }

   #  Standard method to report MTypes served by this object it its
   #  capacity as an agent.
   public method get_subscribed_mtypes {} {
      return {samp.hub.event.shutdown samp.app.ping test.echo}
   }

   #  Implementation for samp.hub.event.shutdown MType.
   public method samp.hub.event.shutdown {sender_id params} {
      puts "SAMP hub stopped"
      set_reginfo_ {}
   }

   #  Implementation for samp.app.ping MType.
   public method samp.app.ping {sender_id params} {
   }

   #  Implementation for test.echo MType.
   public method test.echo {sender_id params} {
      return [rpcvar struct $params]
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
   public method samp_reg_command {cmd} {
      lappend samp_reg_commands_ $cmd
   }

   #  Performs the requested callbacks when the registration status
   #  may have changed.
   protected method inform_samp_reg_ {} {
      foreach cmd $samp_reg_commands_ {
         catch {eval $cmd}
      }
   }

   #  Internal method to store registration information.
   protected method set_reginfo_ {reginfo} {
      if {[string length $reginfo] == 0} {
         set private_key_ {}
      } {
         upvar $reginfo rinfo
         set private_key_ $rinfo(samp.private-key)
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
      foreach ag $user_agents_ {
         lappend ags $ag
      }
      return $ags
   }

   #  Creates a message given the MType and parameters.
   private method message {mtype params} {
      set msg(samp.mtype) $mtype
      set msg(samp.params) $params
      return [array get msg]
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
            plastic::PlasticApp::stop_server
            puts stderr \"GAIA aborts, signal: %S\"
            exit
         "
      }

      #  Log message.
      puts "Started XML-RPC server for PLASTIC at $server_url_"
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
   public variable client_tracker [code [samp::ClientTracker #auto]] {
      $client_tracker configure -hub [code $hub_] -self $client_id_
   }
   public variable client_tracker {}
   public variable metadata {samp.name gaia_app}

   #  Private variables: (available to instance)
   #  -----------------
   private variable hub_
   private variable user_agents_
   private variable private_key_ {}
   private variable samp_reg_commands_ {}

   #  Common variables:
   #  -----------------
   private common server_url_
   private common trapping_ 0
   private common instance_count 0
   public common default_port 8025
}

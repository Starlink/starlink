#+
#  Name:
#     PlasticApp

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Controls an XML-RPC server for use with the PLASTIC protocol.

#  Description:
#     Instances of this class act as PLASTIC listeners.
#     The class provides common methods which start up and shut down
#     the HTTPD server which provides the necessary communications
#     for PLASTIC callbacks.  This server is started up automatically
#     by the first instance that is registered with the hub.

#  Invocations:
#
#     PlasticApp object_name agents [configuration options]
#
#  This creates an instance of a PlasticApp object.  The return is
#  the name of the object.
#
#  The agents constructor argument provides the implementation
#  of the supported messages.  This should contain a list of zero or
#  more objects with methods whose names are the actual message IDs
#  (usually beginning ivo://...) for messages which are supported.
#  The first argument of such methods is the sender_id URL and
#  subsequent arguments are the message parameters.  The return value
#  is the message return.  For example an agent which implements only
#  the ivo://votech.org/test/echo message would look like this:
#
#     itcl::class BasicPlasticAgent {
#        public method ivo://votech.org/test/echo {sender_id text args} {
#           return $text
#        }
#     }
#
#  Note that it is good practice always to add an "args" argument to such
#  methods to mop up additional arguments.  PLASTIC is so specified that
#  extra arguments should be ignored; without the args here a call of
#  the named message with additional arguments would result in an error.
#
#     object_name method arguments
#
#  Performs the given method on this object

#  Configuration options:
#     -app_tracker:
#        An agent which keeps track of the applications currently
#        registered with the hub.  This should usually be a
#        plastic::AppTracker object.

#  Methods:
#     See individual method declarations.

#  Inheritance:
#     This object inherits no other classes.

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  History:
#     11-JUL-2006 (MBT)
#        Original version (server parts cribbed from GaiaXMLRPC).
#     02-OCT-2006 (PWD):
#        Move AuthDefaultFile to user's directory. Cannot be shared
#        between users.
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

itcl::class plastic::PlasticApp {

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

      # Get an object representing a running hub.
      set hub_ [plastic::PlasticHub #auto]

      #  Get the base URL of the XML-RPC server.
      set server_url [get_server_url]

      #  Add some random text as a namespacing measure to ensure that
      #  this object does not receive messages intended for other
      #  listeners.  This also serves as a mild security measure to
      #  prevent hostile clients connecting to the PLASTIC server.
      set rnd [expr [clock clicks] % 10000]
      set subname plastic$rnd
      set listener_url $server_url/$subname

      #  Prepare a procedure which will handle incoming XML-RPC 'perform'
      #  requests, and ensure that such requests get delivered to it.
      #  The procedure is in a namespace specific to this instance.
      set namespace ::plastic::apps::$subname
      namespace eval ${namespace} {
         XMLRPC::export perform
      }
      proc ${namespace}::perform {sender_id msg_id msg_args} "
         return \[$this app_perform \$sender_id \$msg_id \$msg_args\]
      "
      XMLRPC::Domain::register -prefix /$subname -namespace $namespace

      #  Register for communication with the remote hub.
      foreach agent [get_all_agents_] {
         foreach msg [get_supported_messages $agent] {
            set supported_msgs($msg) 1
         }
      }
      set_client_id_ [$hub_ execute registerXMLRPC \
                                    gaia [array names supported_msgs] \
                                    $listener_url]
      configure -app_tracker $app_tracker
      puts "PLASTIC service at $listener_url"
   }

   #  Unregisters this instance with the PLASTIC hub, if it is currently
   #  registered.  If it is not currently registered, nothing happens.
   public method unregister {} {
      if {$client_id_ != ""} {
         if {[catch {
            $hub_ execute unregister $client_id_
         } msg]} {
            puts "Trouble unregistering from PLASTIC: $msg"
         }
         set_client_id_ ""
      }
   }

   #  This method is called when the hub sends a perform call which should
   #  be handled by this instance of the class.  Requests are handled
   #  by handing them off to an appropriate method of one of the agents.
   public method app_perform {sender_id msg_id msg_args} {

      #  Check each agent to see if it has a method with a name that
      #  matches the message ID, and if so execute that with
      #  appropriate arguments.  The message may get passed to more than
      #  one agent if more than one wants to receive it.
      set done 0
      foreach agent [get_all_agents_] {
         foreach func [$agent info function] {
            regsub .*:: $func "" msg_func
            if {$msg_func == $msg_id} {
               set done 1
               set result [eval $agent $msg_func $sender_id $msg_args]
            }
         }
      }

      #  If any of the agents produced a result, return it.
      if {$done} {
         return $result
      }

      #  No luck.  This ought not to happen, since the hub should only
      #  be sending messages with IDs we have previously identified as
      #  names of methods in the agent or this object.
      return "No handler for $msg_id"
   }

   #  Returns a list of the PLASTIC message IDs which are supported by
   #  this listener.  The list is determined by introspection on the
   #  agent object.
   public proc get_supported_messages {agent} {
      set slist ""
      foreach func [$agent info function] {
         regsub .*:: $func "" msg_func
         if {[regexp "^ivo://" $msg_func]} {
            lappend slist $msg_func
         }
      }
      return $slist
   }

   #  Sends a message asynchronously to other registered applications
   #  using PLASTIC.
   #  If the recipients_list is non-empty it gives a list of application
   #  IDs for the applications to send to.  If empty, the message is
   #  broadcast to all.
   public method send_message_async {msg_id arg_list recipient_list} {
      if {[llength $recipient_list] == 0} {
         $hub_ execute requestAsynch $client_id_ $msg_id $arg_list
      } else {
         $hub_ execute requestToSubsetAsynch $client_id_ $msg_id $arg_list \
                                             $recipient_list
      }
   }

   #  Sends a message synchronoously to other registered applications
   #  using PLASTIC.
   #  If the recipients_list is non-empty it gives a list of application
   #  IDs for the applications to send to.  If empty, the message is
   #  broadcast to all.
   public method send_message_sync {msg_id arg_list recipient_list} {
      if {[llength $recipient_list] == 0} {
         return [$hub_ execute request $client_id_ $msg_id $arg_list]
      } else {
         return [$hub_ execute requestToSubset $client_id_ $msg_id $arg_list \
                                               $recipient_list]
      }
   }

   #  Invoked when the HubStopping message is received.
   public method ivo://votech.org/hub/event/HubStopping {sender_id args} {
      puts "PLASTIC hub stopped."
      set_client_id_ ""
   }

   #  Returns a boolean value indicating whether the application is
   #  currently [believed to be] registered with a live hub.
   public method is_registered {} {
      return [expr {$client_id_ != ""}]
   }

   #  Adds a callback command to be executed when the PLASTIC registration
   #  status changes.  An attempt will be made to execute the given command
   #  when this application registers or unregisters with a hub
   #  (or when that may have happened).
   public method plastic_reg_command {cmd} {
      lappend plastic_reg_commands_ $cmd
   }

   #  Performs the requested callbacks when the registration status
   #  may have changed.
   protected method inform_plastic_reg_ {} {
      foreach cmd $plastic_reg_commands_ {
         catch {eval $cmd}
      }
   }

   #  Internal method to set the value of the client_id_ variable.
   #  client_id_ is used internally to indicate whether we are currently
   #  registered with a hub (we're not if it's an empty string).
   protected method set_client_id_ {cid} {
      set client_id_ $cid
      inform_plastic_reg_
   }

   #  Returns a list of all the agents that will be used to respond to
   #  hub requests.
   private method get_all_agents_ {} {
      lappend ags $this $app_tracker
      foreach ag $user_agents_ {
         lappend ags $ag
      }
      return $ags
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

   #  Informs the PLASTIC hub that any instances of this class which
   #  are registered as listeners will no longer be listening, and then
   #  stops the HTTPD server.  If the server is already stopped and
   #  no instances are registered with the hub, does nothing.
   #  Catches its own errors, so is guaranteed(?) to execute cleanly.
   public proc stop_server {} {
      if {[catch {
         foreach app [itcl::find objects -class plastic::PlasticApp] {
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
   public variable app_tracker [code [plastic::AppTracker #auto]] {
      $app_tracker configure -hub [code $hub_] -self $client_id_
   }

   #  Private variables: (available to instance)
   #  -----------------
   private variable hub_
   private variable user_agents_
   private variable client_id_ {}
   private variable plastic_reg_commands_ {}

   #  Common variables:
   #  ----------------
   private common server_url_
   private common trapping_ 0
   public common default_port 8025
}

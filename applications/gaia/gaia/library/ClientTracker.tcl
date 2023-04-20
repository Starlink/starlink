#+
#  Name:
#     ClientTracker

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Keeps track of SAMP clients which are currently registered.

#  Description:
#     An instance of this class responds to SAMP hub administrative
#     messages to the extent necessary for working out what clients
#     are registered and what their subscriptions are.  It is designed
#     to be used as one of the agents of a SampClient.

#  Configuration options:
#     -hub
#        SampHub object with which to communicate.
#     -self_id
#        Public client ID of the application on behalf of which this tracker
#        is running.  This is just used so that this ID can be excluded
#        from returned list of external applications subscribed to some
#        MType.
#     -private_key
#        Private key for hub communication.

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
#     22-JUL-2009: (MBT)
#        Original version, adapted from AppTracker.tcl.
#     {enter_further_changes_here}

#-

itcl::class samp::ClientTracker {

   constructor {args} {
      eval configure $args
   }

   #  Standard method reporting which MTypes are implemented by this agent.
   public method get_subscribed_mtypes {} {
      return {
         samp.hub.event.register
         samp.hub.event.unregister
         samp.hub.event.metadata
         samp.hub.event.subscriptions
      }
   }

   #  Invoked when a client registers with the hub.
   public method samp.hub.event.register {sender_id param_list} {
      array set params $param_list
      set id $params(id)
      set clients_($id) 1
   }

   #  Invoked when a client unregisters from the hub.
   public method samp.hub.event.unregister {sender_id param_list} {
      array set params $param_list
      set id $params(id)
      if {[info exists clients_($id)]} {
         unset clients_($id)
      }
      if {[info exists metadata_($id)]} {
         unset metadata_($id)
      }
      if {[info exists subscriptions_($id)]} {
         unset subscriptions_($id)
      }
      inform_client_change_
   }

   #  Invoked when a client declares its metadata to the hub.
   public method samp.hub.event.metadata {sender_id param_list} {
      array set params $param_list
      set id $params(id)
      set metadata_($id) $params(metadata)
      inform_client_change_
   }

   #  Invoked when a client declares its subscriptions to the hub.
   public method samp.hub.event.subscriptions {sender_id param_list} {
      array set params $param_list
      set id $params(id)
      set subscriptions_($id) $params(subscriptions)
      inform_client_change_
   }

   #  Returns a list of client IDs for clients (excluding self) which
   #  are subscribed to a given MType.  Wildcards are handled correctly.
   public method get_subscribed_clients {mtype} {
      array set subscribed {}
      foreach id [array names clients_] {
         if {$id != $self_id && [info exists subscriptions_($id)]} {
            foreach {sub_mtype sub_info} $subscriptions_($id) {
               if {[mtype_matches_ $sub_mtype $mtype]} {
                  set subscribed($id) 1
               }
            }
         }
      }
      return [lsort -command by_client_name_ [array names subscribed]]
   }

   #  Returns a list of all client IDs (excluding self) currently registered.
   public method get_all_clients {} {
      return [lsort -command by_client_name_ [array names clients_]]
   }

   #  Returns the declared name of a given client.  If no metadata has been
   #  declared for that client, a fake name based on the client ID is used.
   public method get_name {client_id} {
      if {[info exists metadata_($client_id)]} {
         array set meta $metadata_($client_id)
         if {[info exists meta(samp.name)]} {
            return $meta(samp.name)
         }
      }
      append name {[} $client_id {]}
      return $name
   }

   #  Adds a callback command to be executed when the list of registered
   #  clients changes.  An attempt will be made to execute the given
   #  command when other applications register or unregister with the hub,
   #  or when their metadata or subscriptions change.
   public method client_change_command {cmd} {
      lappend client_change_commands_ $cmd
   }

   #  Performs the requested callbacks when the set of registered clients
   #  or their subscriptions or metadata may have changed.
   private method inform_client_change_ {} {
      foreach cmd $client_change_commands_ {
         catch {eval $cmd}
      }
   }

   #  Records the hub object which this tracker is to use.  It won't work
   #  unless this is set to a live hub.
   private method init_hub_ {hub} {
      array unset clients_
      array unset metadata_
      array unset subscriptions_
      foreach id [{*}$hub execute getRegisteredClients $private_key] {
         set clients_($id) 1
         set metadata_($id) [{*}$hub execute getMetadata $private_key $id]
         set subscriptions_($id) \
                [{*}$hub execute getSubscriptions $private_key $id]
      }
   }

   #  Sorting command for clients which uses their name for ordering.
   private method by_client_name_ {client1 client2} {
      set name1 [get_name $client1]
      set name2 [get_name $client2]
      if {$name1 < $name2} {
         return -1
      } elseif {$name1 > $name2} {
         return +1
      } else {
         if {$client1 < $client2} {
            return -1
         } elseif {$client1 > $client2} {
            return +1
         } else {
            return 0
         }
      }
   }

   #  Indicates whether a given subscription key counts as subscribing to
   #  a given concrete MType.  Wildcards are handled.
   private proc mtype_matches_ {sub_mtype mtype} {
      if {$sub_mtype == $mtype} {
         return 1;
      }
      if {$sub_mtype == "*"} {
         return 1;
      }
      if {[string range $sub_mtype end-1 end] == ".*"} {
         set base [string range $sub_mtype 0 end-2]
         set baseleng [string length $base]
         if {[string length $mtype] > $baseleng && \
             [string range $mtype 0 [expr $baseleng-1]] == $base} {
            return 1;
         }
      }
      return 0;
   }

   #  Variables:
   #  ==========
   public variable hub {} {init_hub_ $hub}
   public variable self_id {}
   public variable private_key {}
   private variable clients_
   private variable metadata_
   private variable subscriptions_
   private variable client_change_commands_ {}
}

#+
#  Name:
#     AppTracker

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Keeps track of PLASTIC listeners which are currently registered.

#  Description:
#     An instance of this class responds to PLASTIC housekeeping 
#     messages to the extent necessary for working out what messages
#     can be sent to whom when.  It is designed to be used as one of
#     the agents of a PlasticApp.

#  Configuration options:
#     -self
#        Application ID of the application on behalf of which this tracker
#        is running.  This is just used so that this ID can be excluded
#        from returned lists of external applications which support some
#        functionality.

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
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  History:
#     20-JUL-2006 (MBT)
#        Original version.
#     {enter_further_changes_here}

#-

itcl::class plastic::AppTracker {

   constructor {args} {
      eval configure $args
   }

   #  Invoked when an application registers with the hub.
   public method ivo://votech.org/hub/event/ApplicationRegistered {sender id
                                                                   args} {
      check_hub_
      add_app_ $id
      inform_plastic_apps_
      return $VOID
   }

   #  Invoked when an application unregisters from the hub.
   public method ivo://votech.org/hub/event/ApplicationUnregistered {sender id
                                                                     args} {
      check_hub_
      unset apps_($id)
      inform_plastic_apps_
      return $VOID
   }

   #  Returns a list of external plastic::ApplicationItem objects
   #  which claim to support a given message id.  Any with an ID which
   #  matches the self argument is excluded if necessary.
   public method get_supporting_apps {msg_id} {
      check_hub_
      set supporting {}
      foreach id [array names apps_] {
         if {$id != $self} {
            set app $apps_($id)
            set supported [$app cget -supported_messages]
            if {[lsearch -exact $supported $msg_id] >= 0} {
               lappend supporting [code $app]
            }
         }
      }
      return [lsort -command by_app_name_ $supporting]
   }

   #  Adds an application to the internal list of registered ones.
   protected method add_app_ {id} {
      set apps_($id) \
         [plastic::ApplicationItem #auto \
            -id $id \
            -name [$hub execute getName $id] \
            -supported_messages [$hub execute getUnderstoodMessages $id]]
   }

   #  Checks that we are apparently registered and throws an error otherwise.
   protected method check_hub_ {} {
      if {$hub == ""} {
         error "No hub"
      }
   }

   #  Sorting command for ApplicationItem objects which uses their name
   #  configuration attributes.
   private proc by_app_name_ {app1 app2} {
      set name1 [$app1 cget -name]
      set name2 [$app2 cget -name]
      if {$name1 < $name2} {
         return -1
      } elseif {$name1 > $name2} {
         return +1
      } else {
         return 0
      }
   }

   #  Records the hub object which this tracker is to use.  It won't work
   #  unless this is set to a live hub.
   public variable hub {} {
      if {[array exists apps_]} {
         unset apps_
      }
      foreach id [$hub execute getRegisteredIds] {
         add_app_ $id
      }
      inform_plastic_apps_
   }

   #  Adds a callback command to be executed when the list of registered
   #  applications changes.  An attempt will be made to execute the given
   #  command when other applications register or unregister with the hub.
   public method plastic_apps_command {cmd} {
      lappend plastic_apps_commands_ $cmd
   }

   #  Perform the requested callbacks when the list of registered 
   #  applications has, or may have, changed.
   protected method inform_plastic_apps_ {} {
      foreach cmd $plastic_apps_commands_ {
         catch {eval $cmd}
      }
   }

   #  Variables:
   #  ==========

   public variable self {}

   #  Internal array used to store the currently registered applications.
   #  It is a clientId->ApplicationItem mapping.
   protected variable apps_

   #  Callback commands to execute when apps list changes.
   protected variable plastic_apps_commands_ {}

   #  Return value for XML-RPC methods declared void.
   protected common VOID [list]
}

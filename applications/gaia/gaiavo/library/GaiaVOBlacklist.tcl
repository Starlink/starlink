#+
#  Name:
#     GaiaVOBlacklist

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Manage a list of identifiers that represent blacklisted services.

#  Description:
#     Instances of this class should be used to manage the blacklists for each
#     service type (SIAP, ConeSearch). Services are usually blacklisted because
#     they are known to be broken. Since each service should be uniquely
#     identified by its identifier (an ivorn) we use that to represent a
#     service.
#
#     A blacklist is usually stored in a backing file in the standard
#     ~/.skycat directory. This may have a default equivalent in the
#     distribution (like GaiaSIAPBlacklist), so this object just requires a
#     filename.
#
#     Blacklists are just simple text files with comment lines starting with #
#     in the first column and other lines being the identifiers of the
#     blacklisted services.

#  Invocations:
#
#        GaiaVOBlacklist object_name [configuration options]
#
#     This creates an instance of a GaiaVOBlacklist object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Copyright:
#     Copyright (C) 2008-2009 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     09-JAN-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaiavo::GaiaVOBlacklist {

   #  Inheritances:
   #  -------------

   #  Nothing.

   #  Constructor:
   #  ------------
   constructor {args} {
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor {
   }

   #  Methods:
   #  --------


   #  Check if an identifier is blacklisted. These are URNs of the form
   #  ivo://... that should be unique for each service.
   public method blacklisted {identifier} {
      if { [array exists blacklist_] } {
         return [info exists blacklist_($backingstore,$identifier)]
      }
      return 0
   }

   #  Add an identifier to the blacklist and update the standard file.
   public method blacklist {identifier} {
      set blacklist_($backingstore,$identifier) $identifier
      save_blacklist_
   }

   #  Remove an identifier from the blacklist.
   public method unblacklist {identifier} {
      if { [array exists blacklist_] } {
         if { [info exists blacklist_($backingstore,$identifier)] } {
            unset blacklist_($backingstore,$identifier)
            save_blacklist_
         }
      }
   }

   #  Return a list all the backlisted identifiers.
   public method list {} {
      if { [array exists blacklist_] } {
         set result {}
         foreach {id value} [array get blacklist_ "$backingstore,*"] {
            lappend result $value
         }
         return $result
      }
      return {}
   }

   #  Save the current blacklist to the backing store, if defined.
   protected method save_blacklist_ {} {
      if { [array exists blacklist_] } {
         if { $blackfile_ != {} } {
            set fid [::open $blackfile_ "w"]
            puts $fid "\#  Backlisted SIAP servers. These will never be queried."
            puts $fid "\#"
            foreach {id value} [array get blacklist_ "$backingstore,*"] {
               puts $fid $value
            }
            ::close $fid
         }
      }
   }

   #  Initialise the blacklist from the backing store.
   protected method init_blacklist_ {} {

      #  Look for the blacklist.
      if { ! [::file exists $blackfile_] } {

         #  Not found check for builtin defaults.
         set builtin $::gaiavo_library/$backingstore
         if { [::file exists $builtin] } {
            ::file copy -force  $builtin $blackfile_
         } else {

            #  No file, no builtin so start with an empty list.
            return
         }
      }

      #  Read file.
      set fid [::open $blackfile_ "r"]
      while { [gets $fid line] >= 0 } {
         if { [string range $line 0 0] != "\#" } {
            set blacklist_($backingstore,$line) $line
         }
      }
      ::close $fid
   }

   #  Public entry point for getting a instance of this related to a
   #  particular service type. The service type is defined by the
   #  backing store, which is a short name, SIAP, Cone etc.
   #  The real backingstore is Gaia[SIAP|Cone]Blacklist.
   public proc get_instance {backingstore} {
      if { ! [info exists instances_($backingstore)] } {
         set instances_($backingstore) \
            [uplevel \#0 gaiavo::GaiaVOBlacklist \#auto \
                -backingstore Gaia${backingstore}Blacklist]
      }
      return $instances_($backingstore)
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the backing store file. Will be located in the standard
   #  directory or in the distribution.
   public variable backingstore {} {
      if { $backingstore != {} } {
         set blackfile_ [utilGetConfigFilename .skycat $backingstore]
         init_blacklist_
      } else {
         set blackfile_ {}
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  The file used to store the list.
   protected variable blackfile_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The blacklist of identifiers, indexed by the backingstore name and the
   #  identifier so it is shared when the backingstore name is the same.
   protected common blacklist_

   #  All instances created using get_instance_. Indexed by backingstore.
   protected common instances_
}

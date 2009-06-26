#+
#  Name:
#     GaiaCookie.tcl

#  Purpose:
#     Class for keeping track of a magic number.

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     An instance of this class writes a file in the user's home directory
#     containing a randomly-generated value.  Certain sensitive
#     operations (e.g. execution of arbitrary Tcl code) may require
#     presentation of this value to operate.  This effectively restricts
#     use of such operations to agents which can read the file.
#
#     Since it only makes sense to have a single cookie for the whole
#     application, this is a singleton class.  Use the get_instance
#     class proc to obtain the singleton instance.

#  Invocation:
#     set cookie gaia::GaiaCookie::get_instance

#  Authors:
#     MBT: Mark Taylor
#     {enter_new_authors_here}

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

#  History:
#     17-AUG-2006 (MBT):
#        Original version.
#     {enter_changes_here}

#-

itcl::class gaia::GaiaCookie {

   #  Constructor.
   constructor {args} {
      eval configure $args
      if {[catch {
         set cookie [create_cookie_ 8]
         write_file_ $cookie
      } msg]} {
         puts "No cookie: $msg $::errorInfo"
         set cookie "unknown"
      }
   }

   #  Destructor.
   destructor {
      catch {
         if {[read_file_] == $cookie} {
            file delete [get_path_]
         }
      }
   }


   #  Common procs.
   #  -------------

   #  Returns the singleton instance of this class.
   public proc get_instance {} {
      if {$instance_ == ""} {
         set instance_ [code [GaiaCookie #auto]]
      }
      return $instance_
   }


   #  Protected methods.
   #  ------------------

   #  Returns the fully-qualified path name for the file used by this object.
   protected method get_path_ {} {
      return $::env(HOME)/$filename
   }

   #  Writes a given line to the file associated with this object.
   #  The file will only be overwritten if the overwrite variable is set.
   protected method write_file_ {cookie} {
      set path [get_path_]
      if {[file exists $path]} {
         if {$overwrite} {
            file delete $path
         } else {
            error "File $path already exists - not overwriting"
         }
      }
      set fd [open $path w]
      file attributes $path -permissions rw-------
      puts $fd $cookie
      close $fd
   }

   #  Reads the first line of text in the file used by this object.
   protected method read_file_ {} {
      set path [get_path_]
      if {[file exists $path]} {
         set fd [open [get_path_] r]
         gets $fd line
         close $fd
         return $line
      } else {
         return ""
      }
   }


   #  Protected procs.
   #  ----------------

   #  Generates a random string to be used for the cookie value.
   #  nword is a measure of how long you want it to be
   #  (how many 2-byte groups it contains).
   protected proc create_cookie_ {nword} {
      #  Note: use clock seconds, not clock clicks to get 32 bit value.
      random seed [clock seconds]
      set c [pid]
      for {set i 0} {$i < $nword} {incr i} {
         append c [format -%04x [random 65536]]
      }
      return $c
   }


   #  Instance variables.
   #  -------------------

   #  Name of the file in the user's home directory used for writing the
   #  cookie.
   public variable filename {.gaia-cookie}

   #  Whether to overwrite an existing cookie file if one is unexpectedly
   #  found.
   public variable overwrite {1}

   #  The content of the cookie itself.
   public variable cookie "unassigned"


   #  Common variables.
   #  -----------------

   #  The sole instance of this class.
   protected common instance_ {}
}

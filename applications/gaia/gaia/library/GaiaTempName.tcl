#+
#  Name:
#     GaiaTempName

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Manage and create temporary files in GAIA.

#  Description:
#     Creates a unique set of filenames that describe a set of temporary files
#     for a particular purpose. For instance if the prefix is set to
#     "GaiaClass" and type to ".sdf" then each call to get_name will return a
#     filename "GaiaClass<n>.sdf", where <n> is some unique integer.  If the
#     exists configuration option is set then it is acceptable for the given
#     file to exist already (this is the default), otherwise the name returned
#     will not be that of an existing file.
#
#     All the names generated are stored and the associated disk files
#     can be deleted using the "clear" method (note the unique integer
#     will not be reset), further names can then be generated and
#     stored.
#
#     By default the names are not absolute, so will be created in the default
#     directory, but if the environment variable GAIA_TEMP_DIR is set the names
#     will be qualified using the directory. At least one check will be made to
#     see if the current directory or GAIA_TEMP_DIR is writable and if not
#     GAIA_TEMP_DIR will be set to /tmp.
#
#     If a one off name is required use the make_name method, but that will
#     require a unique integer.

#  Invocations:
#
#        GaiaTempName object_name [configuration options]
#
#     This creates an instance of a GaiaTempName.tcl object. The return is
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

#  Configuration options:

#  Methods:

#  Inheritance:
#     This object inherits no other classes.

#  Copyright:
#     Copyright (C) 2007 Science and Technology Facilities Council
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
#     28-AUG-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaTempName {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval configure $args
      check_writable_
   }

   #  Destructor:
   #  -----------
   destructor  {
      set tmpnames_ {}
   }

   #  Methods:
   #  --------

   #  Get the next temporary name.
   public method get_name {} {
      return [get_new_name_ $prefix $type]
   }

   #  Get the next temporary name, but with a different type.
   public method get_typed_name {tmptype} {
      return [get_new_name_ $prefix $tmptype]
   }

   #  Get the next temporary name with the given prefix and type.
   protected method get_new_name_ {lprefix ltype} {

      #  If we need a name for a non-existent file keep looking until
      #  a free name is found.
      set loop 1
      while { $loop } {
         incr unique_
         set loop 0
         set tmpname [gaia::GaiaTempName::make_name $lprefix $unique_ $ltype]
         if { ! $exists } {
            if { [::file exists $tmpname] } {
               set loop 1
            }
         }
      }
      add_name $tmpname
      return $tmpname
   }

   #  Add a name to the list of temporary files. Will be managed and can
   #  be delete using the clear method.
   public method add_name {name} {
      lappend tmpnames_ "$name"
   }

   #  Clear all the temporary files, deleting any diskfiles.
   public method clear {} {
      foreach f $tmpnames_ {
         if { [::file exists $f] } {
            catch {::file delete -force $f}
         }
      }
      set tmpnames_ {}
   }

   #  Create a temporary file name from a prefix, integer and type.
   #  This will be made absolute if the GAIA_TEMP_DIR variable is set.
   public proc make_name {prefix unique type} {
      check_writable_
      if { [info exists ::env(GAIA_TEMP_DIR)] } {
         set prefix [::file join "$::env(GAIA_TEMP_DIR)" "$prefix"]
      }
      return "${prefix}${unique}${type}"
   }

   #  Check if the designated directory is writable and if not change the
   #  setting of GAIA_TEMP_DIR to TMPDIR or /tmp.
   protected proc check_writable_ {} {
      if { ! $writechecked_ } {
         set writable 0
         if { [info exists ::env(GAIA_TEMP_DIR)] } {
            if { [::file writable $::env(GAIA_TEMP_DIR)] } {
               set writable 1
            }
         } else {
            if { [::file writable "."] } {
               set writable 1
            }
         }
         if { ! $writable } {
            if { [info exists ::env(TMPDIR)] } {
               set ::env(GAIA_TEMP_DIR) $::env(TMPDIR)
            } else {
               set ::env(GAIA_TEMP_DIR) "/tmp"
            }
         }
         set writechecked_ 1
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The prefix for temporary names.
   public variable prefix "GaiaTemp"

   #  The file type.
   public variable type ".sdf"

   #  Whether it is OK to return the name of a file that exists.
   public variable exists 1

   #  Protected variables: (available to instance)
   #  --------------------

   #  Unique counter for generating names. Starts from the process
   #  ID to reduce name clashes, with 1000 spare slots.
   protected variable unique_ [expr int([pid]*1000)]

   #  List of valid names.
   protected variable tmpnames_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Whether writability has been checked.
   common writechecked_ 0

#  End of class definition.
}

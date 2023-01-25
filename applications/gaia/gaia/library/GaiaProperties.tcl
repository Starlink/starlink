#+
#  Name:
#     GaiaProperties.tcl

#  Purpose:
#     Defines a class for controlling persisent application-wide
#     properties.

#  Type of Module:
#     [incr Tcl] class

#  Description:
#     This class maintains a list of "key" "value" pairs that are
#     saved when GAIA exits and restored when GAIA is re-started. It
#     is therefore for use in storing persistent properties that
#     should be re-called.
#
#     A special set of methods are provided for "named" keys. These
#     should be keys that are related for some reason (say they are to
#     do with the configuration of a particular class). Named keys can
#     be returned as a list of names, or applied as configuration
#     options to an object.

#  Invocation:
#     GaiaProperties name [configuration options]

#  Notes:
#     There should be only one instance of this class. Use
#
#        GaiaProperties::instance
#
#     to get the reference. When GAIA exits you will probably need
#     to do:
#
#        delete object [GaiaProperties::instance]
#
#     to make sure that the backing store is updated.

#  Authors:
#     PWD: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  Copyright:
#     Copyright (C) 2003 Central Laboratory of the Research Councils
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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Inherits:
#     Nothing

#  History:
#     21-JAN-2003 (PWD):
#        Original version

#-

itcl::class gaia::GaiaProperties {

   #  Singleton entry point:
   #  ----------------------
   proc instance {} {
      if { $instance_ == {} } {
         set instance_ [gaia::GaiaProperties ::\#auto]
      }
      return $instance_
   }

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Stop extra instances from being instantiated.
      set caller [info level [expr [info level] - 1]]
      if { ![string match "::gaia::GaiaProperties::instance" $caller] } {
         error "GaiaProperties cannot be instantiated - use ::instance proc"
      }

      #  Set name of backing file.
      set properties_file_ [utilGetConfigFilename .skycat properties]

      #  Make sure a file can be saved.
      set dir [file dirname $properties_file_]
      if { ! [file exists $dir] } {
         catch { mkdir $dir }
      }

      #  Restore any saved properties.
      restore_properties
   }

   #  Destructor:
   #  -----------
   destructor {

      #  Does nothing, use save_properties when changes are made.
   }

   #  Methods:
   #  --------

   #  Restore any saved properties from the store.
   public method restore_properties {} {
      if { [file readable $properties_file_] } {

         #  Parse rather than source for security.
         read_file_ $properties_file_
      }
   }

   #  Read the contents of a property file. The format is simply
   #     key = value
   #  So the only special value is the equals sign, which many not
   #  appear in a key. Comment lines start with a # (hash) and are
   #  ignored, as are blank lines.
   protected method read_file_ {filename} {
      set fid [::open $filename r]

      #  Loop over the file, skipping comments and blank lines.
      set full_line_ {}
      set ok 1
      while { $ok  } {
         set llen [gets $fid line]
         if { $llen > 0 } {
            if { ! [string match {\#*} $line] } {
               set split [string first {=} $line]
               if { $split != -1 } {
                  set key [string range $line 0 [expr $split -1]]
                  set key [string trim $key]
                  set value [string range $line [expr $split +1] end]
                  set value [string trim $value]
                  set_property $key $value
               } else {
                  puts stderr "Information: reading global properties: $line"
               }
            }
         } elseif { $llen < 0 } {
            #  End of file
            set ok 0
         }
      }
      ::close $fid
   }

   #  Save any properties to the backing store file.
   public method save_properties {} {
      save_file_ $properties_file_
   }

   #  Save any properties to the backing store.
   protected method save_file_ {filename} {
      if { [info exists values_] } {
         set fid [::open $filename w]
         puts $fid "\# GAIA global properties file."
         puts $fid "\#"
         foreach {key value} [array get values_] {
            puts $fid "$key = $value"
         }
         ::close $fid
      }
   }

   #  Set a property.
   public method set_property {key value} {
      set values_($key) $value
   }
   public method set_named_property {name key value} {
      set key "${name}:${key}"
      set_property $key $value
   }

   #  Get a property. Returns {} if none available.
   public method get_property {key} {
      if { [info exists values_($key)] } {
         return $values_($key)
      }
      return {}
   }
   public method get_named_property {name key} {
      set key "${name}:${key}"
      return [get_property $key]
   }

   #  Unset a property.
   public method unset_property {key} {
      if { [info exists values_($key)] } {
         unset values_($key)
      }
   }
   public method unset_named_property {name key} {
      set key "${name}:${key}"
      unset_property $key
   }

   #  Return a list of property names that match the pattern
   #  "Name:key", given the value of "Name".
   #
   #  A use of this function could be to produce a series of keys
   #  that are bound to a particular widget. In that case named
   #  keys should be created (using the widget class as the name part
   #  of the key).
   public method get_named_keys {name} {
      if { [info exists values_] } {
         return [array names values_ "${name}:*"]
      }
      return {}
   }

   #  Return the key part of a fully named key.
   public method get_unnamed_key {name fullkey} {
      regexp "${name}:(.*)" $fullkey match key
      return $key
   }

   #  Apply any named properties as configuration options to a given
   #  object, i.e. this does:
   #
   #      object configure -$key $value
   #
   #  for all the named properties.
   public method apply_named_properties {object name} {
      if { [info exists values_] } {
         set pattern "${name}:*"
         foreach {key value} [array get values_ $pattern] {
            set newkey [get_unnamed_key $name $key]
            $object configure -${newkey} $value
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The single valid instance of this class.
   private common instance_ {}

   #  The store of key-value pairs.
   protected common values_

   #  The name of the file used to store the properties.
   protected common properties_file_ {}

}

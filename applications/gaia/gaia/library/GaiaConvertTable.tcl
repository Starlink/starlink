#+
#  Name:
#     GaiaConvertTable

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class for running a "foreign" program as catalogue conversion filter.

#  Description:
#     This class defines a object that controls a series of defined
#     filters for converting to and from CAT and ASCII supported formats.

#  Invocations:
#
#        GaiaConvertTable object_name [configuration options]
#
#     This creates an instance of a GaiaConvertTable object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:

#  Methods:

#  Inheritance:
#     This widget inherits no other classes.

#  Copyright:
#     Copyright (C) 1998-2001 Central Laboratory of the Research Councils
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
#     {enter_new_authors_here}

#  History:
#     28-SEP-1998 (PWD):
#        Original version.
#     08-AUG-2001 (PWD):
#        Changed to map file types to lower case.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaConvertTable {

   #  Inheritances:
   #  -------------
   #  Nothing

   #  Constructor:
   #  ------------
   constructor  {args} {

       #  Set the names of the conversion filters.
       global gaia_dir
       foreach type $cattypes_ {
          set to_app_($type) "$gaia_dir/cat2tab"
          set from_app_($type) "$gaia_dir/tab2cat"
          set to_filter_($type) {}
          set from_filter_($type) {}
       }
       foreach type $asciitypes_ {
          set to_app_($type) "$gaia_dir/asc2tab"
          set from_app_($type) "$gaia_dir/tab2asc"
          set to_filter_($type) {}
          set from_filter_($type) {}
       }
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Convert from a CAT/ASCII catalogue to a tab table.
   public method to {in out {now 0}} {

      #  Get the file type.
      set type [get_type_ $in]

      #  Start up filter.
      if { $to_filter_($type) == {} } {
         global env
         set to_filter_($type) [GaiaForeignExec \#auto \
                                   -application $to_app_($type) \
                                   -show_output 0]
      }

      #  Now attempt the conversion.
      set cmd [format $to_cmd_ $in $out]
      if { $now } {
         catch {eval $to_filter_($type) runnow $cmd} msg
         if { $msg == "1" || $msg == "0" } {
            set msg {}
         }
      } else {
         catch {$to_filter_($type) runwiths $cmd} msg
      }
      if { $msg != {} } {
         return 0
      }
      return 1
   }

   #  Convert a tab table to a CAT/ASCII catalogue.
   public method from {in out {now 0}} {

      #  Get the file type so we can invoke the correct filter.
      set type [get_type_ $out]

      #  Start up filter.
      if { $from_filter_($type) == {} } {
         global env
         set from_filter_($type) [GaiaForeignExec \#auto \
                                     -application $from_app_($type) \
                                     -show_output 0]
      }
      
      #  CAT will not overwrite existing files, so do this ourselves.
      if { [file exists $out] } {
         file delete $out
      }

      #  Now attempt the conversion.
      set cmd [format $from_cmd_ $in $out]
      if { $now } {
         set res [catch {eval $from_filter_($type) runnow $cmd} msg]
         if { $msg == "1" || $msg == "0" } {
            set msg {}
         }
      } else {
         set res [catch {$from_filter_($type) runwiths $cmd} msg]
      }
      if { $msg != {} || $res != 0 } {
         if { $msg == {} } {
            set msg "Failed to convert temporary file: $in back to $out"
         }
         error_dialog "$msg"
         return 0
      }
      return 1
   }

   #  Get the type of file. This should match one of our known types
   #  and conversion filters.
   protected method get_type_ {name} {

      #  Extract type from extension and map to lower case.
      set type [string tolower [file extension $name]]
      
      #  Some FITS types may reference extensions... (TODO: gsc etc?).
      if { [string match {.fits*} "$type"] } {
         set type ".fits"
      } elseif { [string match {.fit*} "$type"] } {
         set type ".fit"
      }
      return $type
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Names of applications that form the input/output filters. These
   #  are indexed by the file types.
   protected variable to_app_
   protected variable from_app_

   #  Known file types. These are mapped to lower case.
   protected variable cattypes_ ".fits .fit .gsc .txt"
   protected variable asciitypes_ ".asc .lis"

   #  Command strings to run the convert to/from a tab-table. This
   #  contains "format" specifiers for the input and output names.
   protected variable to_cmd_   "in=%s out=%s accept"
   protected variable from_cmd_ "in=%s out=%s accept"

   #  Names of the objects used to control the filters.
   protected variable to_filter_
   protected variable from_filter_

   #  End of class definition.
}

